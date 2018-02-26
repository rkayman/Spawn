namespace Amber.Spawn

module Scheduler = 

    open System
    open System.Threading
    open Utilities
     
    type ScheduleResult = {
        agentId: Guid;
        scheduleId: Guid;
        cancelToken: CancellationTokenSource;
    } with member this.CancelSchedule() = this.cancelToken.Cancel()

    type private ScheduleCadence = Once | Repeating 

    type private ScheduleMessage<'a> = 
        | Schedule of ScheduleCadence * TimeSpan * ('a -> unit) * 'a * 
                      ScheduleResult ResultMessage AsyncReplyChannel 

    type SchedulingAgent<'a>() = 

        let agentId = Guid.NewGuid() 

        let work repeat delay receiver msg (cts: CancellationTokenSource) =
            let rec loop time (ct: CancellationTokenSource) = async {
                do! Async.Sleep delay
                if ct.IsCancellationRequested then ct.Dispose() 
                else
                    msg |> receiver 
                    if repeat then return! loop time ct
            }
            loop delay cts

        let once = work false 

        let repeat = work true

        let cadenceWorker = function 
            | Once -> once 
            | Repeating -> repeat

        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with 
                | Schedule (cadence, ts, receiver, msg, replyChannel) -> 
                    try
                        let worker = cadenceWorker cadence
                        let delay = int ts.TotalMilliseconds
                        let cts = new CancellationTokenSource()
                        let result = { agentId = agentId; scheduleId = Guid.NewGuid(); cancelToken = cts }
                        Async.StartImmediate(worker delay receiver msg cts)
                        replyChannel.Reply (Success result)
                    with 
                    | ex -> replyChannel.Reply (Error ex.Message)                    
                    return! loop ()                    
            }
            loop ())

        member __.ScheduleAlarm(receiver, msg:'a, ts, isRepeating) = 
            let buildMessage replyChannel = 
                let cadence = if isRepeating then Repeating else Once
                Schedule (cadence, ts, receiver, msg, replyChannel)
            agent.PostAndReply (fun rc -> rc |> buildMessage)
