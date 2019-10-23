namespace Spawn

open Spawn.Clock
open Spawn.Common
open Spawn.Configuration
open Spawn.Messages
open NodaTime
open FSharp.Control.Reactive
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reactive.Concurrency
open System.Threading
open System

module Scheduler =
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
    
    type internal CommandOption = Domain of string option | Name of string option | Id of string option
    
    let private (|ByDomain|ByName|ById|) x =
        match x with
        | Domain domain -> ByDomain domain
        | Name name     -> ByName name
        | Id id         -> ById id
        
    type private AlarmCommand =
        | Schedule of Alarm
        | List of CommandOption * AsyncReplyChannel<AlarmInfo option>
        | Trigger of Instant
        | Identity of AsyncReplyChannel<string>
    
    type private AlarmConfig =
        { id:    Guid
          alarm: Alarm option
          timer: IDisposable option }
    
    let private alarmAccess (timerThread: IScheduler) (log: Logging.Log) (inbox: Agent<_>) =
        let alarmId = Guid.NewGuid()
        let defaultConfig = { id = alarmId; alarm = None; timer = None }
        
        let inline add1 x = x + 1L
        
        let onNext (x: Repeater.PetitSonnerie) = Trigger x.Time |> inbox.Post
        let onError e = (alarmId.ToShortString(), e.ToString()) ||> sprintf "ERROR: [Alarm %s] %A" |> log.Error
        let onCompleted _ = alarmId.ToShortString() |> sprintf "COMPLETED: [Alarm %s]" |> log.Debug
        
        let inline disposeMaybe disposable =
            let dispose (x: IDisposable) = x.Dispose()
            disposable |> Option.iter dispose
        
        let startTimer alarm =
            (alarm.schedule, Alarm.Starting None)
            |> Alarm.Repeat
            |> Alarm.scheduleOn timerThread None
            |> Observable.subscribeWithCallbacks onNext onError onCompleted
                    
        let mutable cnt = 0
        let rec loop config info = async {
            try
                let! msg = inbox.Receive()
                match msg with
                | Schedule ax ->
                    disposeMaybe config.timer
                    let timer = startTimer ax
                    let config' = { config with alarm = Some ax; timer = Some timer }
                    let info' = Some { id = alarmId; domain = ax.domain; name = ax.name; count = 0L; last = None }
                    alarmId.ToShortString() |> sprintf "Scheduled alarm %s" |> log.Debug
                    return! loop config' info'
                
                | List (option, channel) ->
                    match config.alarm with
                    | None -> channel.Reply(None)
                    | Some ax ->
                        match option with
                        | ByDomain value -> ax.domain |> matches value
                        | ByName value -> ax.name |> matches value
                        | ById value -> alarmId.ToShortString() |> matches value
                        |> function
                            | true  -> channel.Reply(info)
                            | false -> channel.Reply(None)
                    return! loop config info
                                        
                | Trigger instant ->
                    let info' = info |> Option.map (fun x -> { x with count = add1 x.count; last = Some instant })
                    (config.id.ToShortString(), instant) ||> sprintf "Triggered alarm %s at %A" |> log.Info
                    return! loop config info'
                    
                | Identity channel ->
                    let id = config.id.ToShortString()
                    channel.Reply(id)
                    id |> sprintf "Read alarm id %s" |> log.Debug
                    return! loop config info
                    
            finally
                if cnt < 1 then
                    config.id.ToShortString() |> sprintf "Completing Alarm agent: %s..." |> log.Info
                    cnt <- cnt + 1
                    disposeMaybe config.timer
                    config.id.ToShortString() |> sprintf "Completed Alarm agent: %s" |> log.Info
        }
        loop defaultConfig None
        
    let alarmLog mgr = Logging.Log(mgr, "Spawn.Scheduler.AlarmActor")
    type private AlarmActor(timerThread, logMgr, ?token: CancellationToken) =
        inherit AutoCancelActor<AlarmCommand>(alarmAccess timerThread (alarmLog logMgr),
                                              alarmLog logMgr,
                                              ?token = token)        
        with
            member this.Id with get() = let buildMessage = fun ch -> Identity ch
                                        this.Agent.PostAndReply(buildMessage)
            
            member this.Schedule(alarm) = this.Agent.Post(Schedule alarm)
            
            member this.ListIf(filter) =
                let buildMessage = fun ch -> List (filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
                
            
    type internal AgendaCommand =
        | Process of Agenda * AsyncReplyChannel<int>
        | Remove of CommandOption * AsyncReplyChannel<int>
        | List of CommandOption * AsyncReplyChannel<seq<AlarmInfo>>
    
    type private AlarmValue = AlarmActor
    type private AlarmDB = ConcurrentDictionary<AlarmKey, AlarmValue>
    type private AlarmKVP = KeyValuePair<AlarmKey, AlarmValue>
    
    let agendaLog mgr = Logging.Log(mgr, "Spawn.Scheduler.AgendaActor")
    let private agendaAccess token logMgr (inbox: Agent<_>) =
        let log = agendaLog logMgr
        let alarms = AlarmDB(Environment.ProcessorCount, 1200)
        let timerThread = new EventLoopScheduler()
        
        let logRequest = function
            | Process (x, _) -> x.alarms.Length |> sprintf "Process %d alarms" |> log.Debug
            | Remove (filter, _) -> filter |> sprintf "Remove %A" |> log.Debug
            | List (filter, _)   -> filter |> sprintf "List %A" |> log.Debug
        
        let removeAlarms (filter: AlarmKVP -> bool) (xs: AlarmDB) =
            let remove cnt (value: IDisposable) = value.Dispose(); cnt + 1
            let numRemoved =
                xs |> Seq.filter filter
                   |> Seq.map (fun t -> xs.TryRemove(t.Key))
                   |> Seq.filter fst
                   |> Seq.map snd
                   |> Seq.fold (fun s t -> remove s t) 0
            numRemoved, xs
            
        let domainFilter value (x: AlarmKVP) = x.Key.domain |> matches value
        let nameFilter value (x: AlarmKVP) = x.Key.name |> matches value
        let idFilter value (x: AlarmKVP) = x.Value.Id |> matches value

        let folder (s: AlarmDB * int) (t: Alarm) =
            let key = { domain = t.domain; name = t.name }
            let dict, cnt = s
            let mutable cnt' = cnt
            let makeValue _ = cnt' <- cnt+1; new AlarmActor(timerThread, logMgr, token)
            let actor = dict.GetOrAdd(key, makeValue)
            actor.Schedule(t)
            dict, cnt'
        
        let mutable cnt = 0
        let rec loop xs = async {
            try
                let! msg = inbox.Receive()
                logRequest msg
                match msg with
                | Process (agenda, channel) ->
                    let xs', cnt = agenda.alarms |> Seq.fold folder (xs, 0)
                    xs'.Count |> sprintf "Processed %d distinct alarms" |> log.Debug
                    channel.Reply(cnt)
                    return! loop xs'
                    
                | Remove (option, channel) ->
                    let filter =  match option with
                                  | ByDomain value -> domainFilter value
                                  | ByName value   -> nameFilter value
                                  | ById value     -> idFilter value
                    let numRemoved, xs' = xs |> removeAlarms filter
                    numRemoved |> sprintf "Removed %d alarms" |> log.Debug
                    channel.Reply(numRemoved)
                    return! loop xs'

                | List (option, channel) ->
                    let! results = xs |> Seq.map (fun t -> t.Value.ListIf(option))
                                      |> Async.Parallel
                    let results' = results |> Seq.filter (Option.isSome) |> Seq.map (Option.get)
                    results' |> Seq.length |> sprintf "List found %d alarms" |> log.Debug
                    channel.Reply(results')
                    return! loop xs
                
            finally
                if cnt < 1 then
                    sprintf "Completing Agenda agent..." |> log.Info
                    cnt <- cnt + 1
                    timerThread.Dispose()
                    sprintf "Completed Agenda agent" |> log.Info
                
        }
        loop alarms
        
    type internal AgendaActor(logMgr, token) =
        inherit AutoCancelActor<AgendaCommand>(agendaAccess token logMgr,
                                               agendaLog logMgr,
                                               token)
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
                
            member this.RemoveId(id) =
                let buildMessage = fun ch -> Remove (Id id, ch)
                this.Agent.PostAndAsyncReply(buildMessage)

            member this.ListDomain(filter) =
                let buildMessage = fun ch -> List (Domain filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
                
            member this.ListName(filter) =
                let buildMessage = fun ch -> List (Name filter, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
                
            member this.ListId(id) =
                let buildMessage = fun ch -> List (Id id, ch)
                this.Agent.PostAndAsyncReply(buildMessage)
    