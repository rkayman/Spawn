namespace Amber.Spawn

module Alarm = 

    open System
    open System.Threading
    open Utilities

    type AlarmCadence = Once | Repeating
     
    type Alarm<'T> = {
        agentId: Guid;
        messageId: Guid
        alarmId: Guid;
        cancelToken: CancellationTokenSource;
        frequency: AlarmCadence * TimeSpan;
        payload: 'T
    } with
        member this.CancelSchedule() = this.cancelToken.Cancel()
        member this.GetMessageContext() = 
            { new IMessageContext with
              member __.AgentId with get() = this.agentId 
              member __.MessageId with get() = this.messageId
              member __.ActivityId with get() = this.alarmId    // TODO: should be the ID assigned to configuration 
              member __.CausationId with get() = this.alarmId 
              member __.CorrelationId with get() = this.agentId }
        static member Build<'T>(idAgent, idSchedule, tokenCancel, (options: AlarmSettings<'T>)) =
            { messageId = Guid.NewGuid(); agentId = idAgent; alarmId = idSchedule; 
              cancelToken = tokenCancel; frequency = options.frequency; 
              payload = options.payload }
    and AlarmSettings<'T> = {
        frequency: AlarmCadence * TimeSpan;
        payload: 'T
        handler: AlarmHandler<'T>
    } and AlarmHandler<'T> = Alarm<'T> -> unit

    type AlarmResult<'T> = 
        | Scheduled of DateTimeOffset * Alarm<'T>
        | Cancelled of DateTimeOffset * Guid * string
        | Alarms of Guid * Alarm<'T> list
        | Stopped of DateTimeOffset * string
        | Error of DateTimeOffset * string * Exception option

    type private AlarmMessage<'T> = 
        | Schedule of AlarmSettings<'T> * AlarmResult<'T> AsyncReplyChannel
        | Cancel of Guid option * AlarmResult<'T> AsyncReplyChannel
        | List of AlarmResult<'T> AsyncReplyChannel
        | Stop of AlarmResult<'T> AsyncReplyChannel

    type AlarmAgent<'T>() = 
        let agentId = Guid.NewGuid() 

        let mutable alarms: Map<Guid, Alarm<'T>> = Map.empty

        let timer repeat delay (handler: AlarmHandler<'T>) (args: Alarm<'T>) =
            let rec loop time (ct: CancellationTokenSource) = async {
                do! Async.Sleep delay
                if ct.IsCancellationRequested then ct.Dispose() 
                else
                    // TODO: Make async
                    handler args
                    if repeat then return! loop time ct 
                    else return! async.Zero()
            }
            loop delay args.cancelToken

        let once = timer false 

        let repeat = timer true

        let cadenceWorker = function 
            | Once -> once 
            | Repeating -> repeat

        let stopAlarm id = 
            match alarms.TryFind id with
            | Some alarm -> alarm.CancelSchedule()
            | _ -> invalidArg "id" (sprintf "Schedule [%A] does not exist" id)

        let stopAllAlarms() = 
            alarms |> Map.iter (fun _ a -> a.CancelSchedule())
            alarms <- Map.empty

        let buildAlarm options =
            let cts = new CancellationTokenSource()
            let scheduleId = Guid.NewGuid()
            Alarm.Build(agentId, scheduleId, cts, options)

        let scheduleAlarm options =
            let (cadence, ts) = options.frequency
            let handler = options.handler
            let worker = cadenceWorker cadence
            let delay = int ts.TotalMilliseconds
            let alarm = buildAlarm options
            alarms <- alarms.Add(alarm.alarmId, alarm)
            Async.Start (worker delay handler alarm)
            alarm


        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with 
                | Schedule (options, ch) -> 
                    try
                        (now(), scheduleAlarm options) 
                        |> Scheduled 
                        |> ch.Reply
                    with
                    | ex ->  Error (now(), ex.Message, Some ex) |> ch.Reply
                    return! loop ()

                | Cancel (id, ch) -> 
                    let guid, msg = match id with
                                    | Some guid -> stopAlarm guid; (guid, (sprintf "Schedule %A stopped" guid))
                                    | None -> stopAllAlarms (); (Guid.Empty, ("All schedules stopped"))
                    ch.Reply <| Cancelled (now(), guid, msg)
                    return! loop ()

                | List ch ->
                    let lst = alarms |> Map.toSeq |> Seq.map snd |> Seq.toList
                    (agentId, lst) |> Alarms |> ch.Reply

                | Stop ch -> 
                    let msg = sprintf "Stopping Alarm {%A}; Cancelling %d schedules; %i Messages remaining in queue." 
                                agentId alarms.Count inbox.CurrentQueueLength
                    stopAllAlarms()
                    Stopped (now(), msg) |> ch.Reply
                    return! async.Zero()
            }
            loop ())

        member __.Id with get() = agentId

        member __.Schedule(alarmOptions) =
            let buildMessage replyChannel = Schedule (alarmOptions, replyChannel)
            agent.PostAndReply buildMessage

        member __.Stop() = 
            agent.PostAndReply (Stop)

        member __.CancelSchedule(?scheduleId) = 
            let buildMessage replyChannel = Cancel (scheduleId, replyChannel)
            agent.PostAndReply buildMessage

        member __.ListSchedules() = 
            agent.PostAndReply (List)
