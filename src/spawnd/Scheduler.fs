namespace Spawn

open Spawn.Clock
open Spawn.IO.Configuration
open NodaTime
open FSharp.Control.Reactive
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reactive.Concurrency
open System.Threading
open System

module Scheduler =

    type Agent<'T> = MailboxProcessor<'T>
    
    type internal AutoCancelActor<'T>(body: (Agent<'T> -> Async<unit>), ?token: CancellationToken) =
        let cts = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource(t))
                        |> Option.defaultValue (new CancellationTokenSource())
        let mutable disposed = false
        
        let actor = Agent<'T>.Start(body, cts.Token)
        
        member internal this.Agent with get() = actor
        
        member internal this.Token with get() = cts.Token
        
        abstract member Dispose : unit -> unit
        default this.Dispose() = (this :> IDisposable).Dispose()

        interface IDisposable with
            member this.Dispose() =
                if not disposed then
                    disposed <- true
                    cts.Cancel()
                    (actor :> IDisposable).Dispose()
                    cts.Dispose()

    let inline private equals (value: string) (source: string) =
        source.Equals(value, StringComparison.OrdinalIgnoreCase)
        
    let inline private contains (value: string) (source: string) =
        source.Contains(value, StringComparison.OrdinalIgnoreCase)
              
    let inline private startsWith (value: string) (source: string) =
        source.StartsWith(value, StringComparison.OrdinalIgnoreCase)
              
    let inline private endsWith (value: string) (source: string) =
        source.EndsWith(value, StringComparison.OrdinalIgnoreCase)

    let private matches (value: string option) (source: string) =
        match value with
        | None -> true
        | Some x when x.Equals("*") -> true
        | Some x ->
            let arr = x.Split('*', StringSplitOptions.None) |> Array.toList
            match arr with
            | x :: y :: [] when x.Length > 0 && y.Length > 0 ->
                source |> startsWith x && source |> endsWith y
            | x :: y :: [] when String.IsNullOrWhiteSpace(x) && y.Length > 0 ->
                source |> endsWith y
            | x :: y :: [] when String.IsNullOrWhiteSpace(y) && x.Length > 0 ->
                source |> startsWith x
            | x :: y :: [] when String.IsNullOrWhiteSpace(x) && String.IsNullOrWhiteSpace(y) ->
                true
            | x :: y :: z :: [] when String.IsNullOrWhiteSpace(x) && String.IsNullOrWhiteSpace(z) ->
                source |> contains y
            | x :: [] when x.Length > 0 ->
                source |> equals x
            | _ -> invalidArg "value" "Filter is missing or illegal"
    
    type internal CommandOption = Domain of string option | Name of string option
    
    let private (|ByDomain|ByName|) x =
        match x with
        | Domain domain -> ByDomain domain
        | Name name     -> ByName name
        
    type private AlarmCommand =
        | Schedule of Alarm
        | Info of CommandOption * AsyncReplyChannel<(Guid * AlarmKey) option>
        | Trigger of Instant 
    
    type private AlarmConfig =
        { id: Guid
          alarm: Alarm option
          timer: IDisposable option }
        
    type System.Guid with
        member x.ToShortString() = x.ToString("N").Substring(19)
    
    let private alarmAccess (timerThread: IScheduler) (log: Agent<_>) (inbox: Agent<_>) =
        let alarmId = Guid.NewGuid()
        let defaultConfig = { id = alarmId; alarm = None; timer = None }
        
        let onNext (x: Repeater.PetitSonnerie) = Trigger x.Time |> inbox.Post
        let onError e = (alarmId.ToShortString(), e.ToString()) ||> sprintf "ERROR [Alarm %s] %A" |> log.Post
        let onCompleted _ = alarmId.ToShortString() |> sprintf "COMPLETED [Alarm %s]" |> log.Post
        
        let startTimer alarm = async {
            let result = (alarm.schedule, Alarm.Starting None)
                         |> Alarm.Repeat
                         |> Alarm.scheduleOn timerThread None
                         |> Observable.subscribeWithCallbacks onNext onError onCompleted
            return result
        }
        
        let mutable cnt = 0
        let rec loop config = async {
            
            try
                let! msg = inbox.Receive()
                match msg with
                | Schedule ax ->
                    let! result = startTimer ax
                    let config' = { config with alarm = Some ax; timer = Some result }
                    //let config' = { config with alarm = Some ax; timer = None }
                    return! loop config'
                
                | Info (option, channel) ->
                    match config.alarm with
                    | None -> channel.Reply(None)
                    | Some ax ->
                        match option with
                        | ByDomain value -> ax.domain |> matches value
                        | ByName value -> ax.name |> matches value
                        |> function
                            | true -> channel.Reply(Some (alarmId, { domain = ax.domain; name = ax.name }))
                            | false -> channel.Reply(None)
                    return! loop config
                    
                | Trigger instant ->
                    (alarmId.ToShortString(), instant) ||> sprintf "INFO [Alarm %s] triggered at %A" |> log.Post
                    return! loop config
                    
            finally
                if cnt < 1 then
                    config.timer |> Option.iter (fun t -> t.Dispose())
                    alarmId.ToShortString() |> sprintf "Completed Alarm agent: %s" |> log.Post
                    cnt <- cnt + 1
        }
        loop defaultConfig
            
    type private AlarmActor(timerThread, log, ?token: CancellationToken) =
        inherit AutoCancelActor<AlarmCommand>(alarmAccess timerThread log, ?token = token)
        
        with
            member this.Schedule(alarm) = this.Agent.Post(Schedule alarm)
            
            member this.InfoFilteredBy(filter) =
                let buildMessage = fun ch -> Info (filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
        
    type internal AgendaCommand =
        | Process of Agenda * AsyncReplyChannel<int>
        | Remove of CommandOption * AsyncReplyChannel<int>
        | List of CommandOption * AsyncReplyChannel<seq<Guid * AlarmKey>>
    
    type private AlarmValue = AlarmActor
    type private AlarmDB = ConcurrentDictionary<AlarmKey, AlarmValue>
    type private AlarmKVP = KeyValuePair<AlarmKey, AlarmValue>
    
    let private agendaAccess log token (inbox: Agent<_>) =
        let alarms = AlarmDB(4, 1200)
        let timerThread = new EventLoopScheduler()
        
        let removeAlarms (filter: AlarmKVP -> bool) (xs: AlarmDB) =
            let remove cnt (value: IDisposable) = value.Dispose(); cnt + 1
            let numRemoved =
                xs |> Seq.filter filter
                   |> Seq.map (fun t -> xs.TryRemove(t.Key))
                   |> Seq.filter fst
                   |> Seq.fold (fun s (_,t) -> remove s t) 0
            numRemoved, xs

        let folder (s: AlarmDB * int) (t: Alarm) =
            let key = { domain = t.domain; name = t.name }
            let dict, cnt = s
            let makeValue _ = new AlarmActor(timerThread, log, token)
            let actor = dict.GetOrAdd(key, makeValue)
            actor.Schedule(t)
            dict, cnt + 1
        
        let mutable cnt = 0
        let rec loop xs = async {
            
            try
                let! msg = inbox.Receive()
                match msg with
                | Process (agenda, channel) ->
                    let xs', cnt = agenda.alarms |> Seq.fold folder (xs, 0)
                    channel.Reply(cnt)
                    return! loop xs'
                
                | Remove (ByDomain value, channel) ->
                    let numRemoved, xs' =
                        xs |> removeAlarms (fun x -> x.Key.domain |> matches value)
                    channel.Reply(numRemoved)
                    return! loop xs'
                
                | Remove (ByName value, channel) ->
                    let numRemoved, xs' =
                        xs |> removeAlarms (fun x -> x.Key.name |> matches value)
                    channel.Reply(numRemoved)
                    return! loop xs'

                | List (option, channel) ->
                    let! results = xs |> Seq.map (fun t -> t.Value.InfoFilteredBy(option))
                                      |> Async.Parallel
                    results |> Seq.filter (Option.isSome) |> Seq.map (Option.get) |> channel.Reply
                    return! loop xs
                
            finally
                if cnt < 1 then
                    timerThread.Dispose()
                    sprintf "Completed Agenda agent" |> log.Post
                    xs |> Seq.iter (fun t -> t.Value.Dispose())
                    cnt <- cnt + 1
                
        }
        loop alarms
        
    type internal AgendaActor(log, token) =
        inherit AutoCancelActor<AgendaCommand>(agendaAccess log token, token)
        
        with
            member this.Process(agenda) =
                let buildMessage = fun ch -> Process (agenda, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
        
            member this.RemoveDomain(filter) =
                let buildMessage = fun ch -> Remove (Domain filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)

            member this.RemoveName(filter) =
                let buildMessage = fun ch -> Remove (Name filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)

            member this.ListDomain(filter) =
                let buildMessage = fun ch -> List (Domain filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
                
            member this.ListName(filter) =
                let buildMessage = fun ch -> List (Name filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
    