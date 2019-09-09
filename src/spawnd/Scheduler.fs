namespace Spawn

open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open System

module Scheduler =
    open Spawn.IO.Configuration

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

    let private contains (value: string option) (source: string) =
        value |> Option.map (fun x -> source.Contains(x, StringComparison.OrdinalIgnoreCase))
              |> Option.defaultValue true
    
    type internal CommandOption = Domain of string option | Name of string option
    
    let private (|ByDomain|ByName|) x =
        match x with
        | Domain domain -> ByDomain domain
        | Name name     -> ByName name
        
    type private AlarmCommand =
        | Schedule of Alarm
        | Info of CommandOption * AsyncReplyChannel<(Guid * AlarmKey) option>
            
    let private alarmStorage (inbox: Agent<_>) =
        let alarmId = Guid.NewGuid()
        
        let mutable cnt = 0
        let rec loop id alarm = async {
            
            try
                let! msg = inbox.Receive()
                match msg with
                | Schedule ax ->
                    return! loop id (Some ax)
                
                | Info (option, channel) ->
                    match alarm with
                    | None -> channel.Reply(None)
                    | Some ax ->
                        match option with
                        | ByDomain value -> ax.domain |> contains value
                        | ByName value -> ax.name |> contains value
                        |> function
                            | true -> channel.Reply(Some (alarmId, { domain = ax.domain; name = ax.name }))
                            | false -> channel.Reply(None)
                    return! loop id alarm
                    
            finally
                if cnt < 1 then
                    alarmId.ToString("N").Substring(19) |> printfn "Completed Alarm agent: %s"
                    cnt <- cnt + 1
        }
        loop alarmId None
            
    type private AlarmActor(?token: CancellationToken) =
        inherit AutoCancelActor<AlarmCommand>(alarmStorage, ?token = token)
        
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
    
    let private alarmAccess (token: CancellationToken) (inbox: Agent<_>) =
        let alarms = AlarmDB(4, 1200)
        
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
            let makeValue _ = new AlarmActor(token)
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
                        xs |> removeAlarms (fun x -> x.Key.domain |> contains value)
                    channel.Reply(numRemoved)
                    return! loop xs'
                
                | Remove (ByName value, channel) ->
                    let numRemoved, xs' =
                        xs |> removeAlarms (fun x -> x.Key.name |> contains value)
                    channel.Reply(numRemoved)
                    return! loop xs'

                | List (option, channel) ->
                    let! results = xs |> Seq.map (fun t -> t.Value.InfoFilteredBy(option))
                                      |> Async.Parallel
                    results |> Seq.filter (Option.isSome) |> Seq.map (Option.get) |> channel.Reply
                    return! loop xs
                
            finally
                if cnt < 1 then
                    printfn "Completed Agenda agent"
                    xs |> Seq.iter (fun t -> t.Value.Dispose())
                    cnt <- cnt + 1
                
        }
        loop alarms
        
    type internal AgendaActor(token) =
        inherit AutoCancelActor<AgendaCommand>(alarmAccess token, token)
        
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
    