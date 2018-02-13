namespace Amber.Spawn

module Agent = 

    open System
    open System.Threading
     
    //Agent alias for MailboxProcessor
    type Agent<'T> = MailboxProcessor<'T>
     
    /// Two types of Schedule messages that can be sent
    type ScheduleMessage<'a> =
        | ScheduleRecurring of ('a -> unit) * 'a * TimeSpan * TimeSpan * CancellationTokenSource AsyncReplyChannel
        | ScheduleOnce of ('a -> unit) * 'a * TimeSpan * CancellationTokenSource AsyncReplyChannel
     
    /// An Agent based scheduler
    type SchedulerAgent<'a>() = 

        let scheduleOnce delay msg receiver (cts: CancellationTokenSource) = async { 
            do! Async.Sleep delay
            if cts.IsCancellationRequested then cts.Dispose() else msg |> receiver 
        }

        let scheduleRecurring initialDelay msg receiver delayBetween cts = 
            let rec loop time (cts: CancellationTokenSource) = async { 
                do! Async.Sleep time
                if cts.IsCancellationRequested then cts.Dispose() else msg |> receiver
                return! loop delayBetween cts
            }
            loop initialDelay cts

        let scheduler = Agent.Start(fun inbox ->
            let rec loop() = async {
                let! msg = inbox.Receive()
                let cts = new CancellationTokenSource()
                match msg with
                | ScheduleRecurring(receiver, msg:'a, initialDelay, delayBetween, replyChan) ->
                    Async.StartImmediate(scheduleRecurring
                                 (int initialDelay.TotalMilliseconds)
                                 msg
                                 receiver
                                 (int delayBetween.TotalMilliseconds)
                                 cts)
                    replyChan.Reply cts
                    return! loop()
                | ScheduleOnce(receiver, msg:'a, delay, replyChan) ->
                    Async.StartImmediate(scheduleOnce
                                 (int delay.TotalMilliseconds)
                                 msg
                                 receiver
                                 cts)
                    replyChan.Reply cts
                    return! loop()
            }
            loop())

        /// Schedules a message to be sent to the receiver after the initialDelay.
        ///  If delaybetween is specified then the message is sent recurringly after the delaybetween interval.
        member this.Schedule(receiver, msg, initialDelay, ?delayBetween) =
            let buildMessage replyChan =
                match delayBetween with
                | Some(x) -> ScheduleRecurring(receiver, msg, initialDelay, x, replyChan)
                | _ -> ScheduleOnce(receiver, msg, initialDelay, replyChan)
            scheduler.PostAndReply (fun replyChan -> replyChan |> buildMessage)
