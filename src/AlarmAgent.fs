namespace Amber.Spawn

module Alarm = 

    open System
    open System.Threading
    open Utilities

    type ScheduleCadence = Once | Repeating 

    type AlarmInfo = {
        frequency: ScheduleCadence * TimeSpan;
        payload: obj
    }
     
    type ScheduledInfo = {
        alarmId: Guid;
        scheduleId: Guid;
        configId: Guid;
        cancelToken: CancellationTokenSource;
        frequency: ScheduleCadence * TimeSpan;
        payload: obj
    } with
        member this.CancelSchedule() = this.cancelToken.Cancel()
        member this.GetMessageContext() = 
            { new IMessageContext with
              member __.AgentId with get() = this.alarmId 
              member __.ActivityId with get() = this.configId 
              member __.CausationId with get() = this.scheduleId 
              member __.CorrelationId with get() = this.alarmId }
        static member Create(idAgent, idSchedule, idConfig, tokenCancel, (alarmInfo: AlarmInfo)) =
            { alarmId = idAgent; scheduleId = idSchedule; 
              configId = idConfig; cancelToken = tokenCancel; 
              frequency = alarmInfo.frequency; payload = alarmInfo.payload }

    type AlarmHandler = ScheduledInfo -> unit

    type AlarmResult = 
        | Scheduled of ScheduledInfo
        | Stopped of DateTimeOffset * string
        | Error of string * Exception option

    type private AlarmMessage<'T> = 
        | Schedule of AlarmInfo * AlarmHandler * AlarmResult AsyncReplyChannel
        | Stop of AlarmResult AsyncReplyChannel

    type AlarmAgent() = 
        // TODO: add dictionary of all schedules so we can clean up on STOP request
        let agentId = Guid.NewGuid() 

        let timer repeat delay (handler: AlarmHandler) (args: ScheduledInfo) =
            let rec loop time (ct: CancellationTokenSource) = async {
                do! Async.Sleep delay
                if ct.IsCancellationRequested then ct.Dispose() 
                else
                    handler args    // TODO: Make async
                    if repeat then return! loop time ct
            }
            loop delay args.cancelToken

        let once = timer false 

        let repeat = timer true

        let cadenceWorker = function 
            | Once -> once 
            | Repeating -> repeat

        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with 
                | Schedule (alarmInfo, handler, replyChannel) -> 
                    try
                        let (cadence, ts) = alarmInfo.frequency
                        let worker = cadenceWorker cadence
                        let delay = int ts.TotalMilliseconds
                        let cts = new CancellationTokenSource()
                        let result = ScheduledInfo.Create(agentId, Guid.NewGuid(), Guid.NewGuid(), cts, alarmInfo)
                        Async.StartImmediate(worker delay handler result)
                        replyChannel.Reply (Scheduled result)
                    with 
                    | ex -> replyChannel.Reply (Error (ex.Message, Some ex))
                    return! loop ()

                | Stop replyChannel -> 
                    let msg = sprintf "Stopping Scheduler {%A} with %i messages in queue. ** Alarms may still be running! **" 
                                agentId inbox.CurrentQueueLength
                    replyChannel.Reply (Stopped (DateTimeOffset.Now, msg))
                    return! async.Zero()
            }
            loop ())

        member __.Id with get() = agentId

        member __.ScheduleAlarm(alarmInfo, handler) = 
            let buildMessage replyChannel = Schedule (alarmInfo, handler, replyChannel)
            agent.PostAndReply buildMessage
