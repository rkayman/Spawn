namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.Threading
    open CommandLine
    open Configuration

    type ResultMessage<'a> = 
        | Success of 'a
        | Error of string 

    type ScheduleResult = {
        agentId: Guid;
        scheduleId: Guid;
        cancelToken: CancellationTokenSource;
    } with member this.CancelSchedule() = this.cancelToken.Cancel()

    type private ScheduleCadence = Once | Repeating 

    type private FooMessage<'a> = 
        | Schedule of ScheduleCadence * ('a -> unit) * 'a * TimeSpan * 
                      ScheduleResult ResultMessage AsyncReplyChannel 

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent<'a>() = 

        let agentId = Guid.NewGuid() 

        let work repeat receiver msg delay (cts: CancellationTokenSource) =
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
                | Schedule (cadence, receiver, msg, ts, replyChannel) -> 
                    try
                        let worker = cadenceWorker cadence
                        let delay = int ts.TotalMilliseconds
                        let cts = new CancellationTokenSource()
                        let result = { agentId = agentId; scheduleId = Guid.NewGuid(); cancelToken = cts }
                        Async.StartImmediate(worker receiver msg delay cts)
                        replyChannel.Reply (Success result)
                    with 
                    | ex -> replyChannel.Reply (Error ex.Message)                    
                    return! loop ()                    
            }
            loop ())

        member __.ScheduleAlarm(receiver, msg:'a, ts, isRepeating) = 
            let buildMessage replyChannel = 
                let cadence = if isRepeating then Repeating else Once
                Schedule (cadence, receiver, msg, ts, replyChannel)
            agent.PostAndReply (fun rc -> rc |> buildMessage)


    [<EntryPoint>]
    let main argv =
        let args = Array.toList argv
        let result = parse args
        let printHelp() = helpMsg |> eprintfn "%s" 

        match result with 
        | Help -> printHelp()
        
        | Errors errors -> 
            errors.PrintErrors 
            printHelp()

        | Options options -> 
            eprintfn "\nUsing configuration found in %A...\n" options.file.Value.FullName 
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            let config = getConfiguration <| Option.get options.file
            printfn "%A" config

            let showHelp = ["help\t\tShow this help message";
                            "once\t\tCreate a one-time timer";
                            "repeat\t\tCreate a repeating timer"; 
                            "stop\t\tStop schedule";
                            "quit\t\tQuit program"]

            let printReceiver msg = printfn "%s" msg

            let schedules = new Dictionary<uint32, ScheduleResult>()

            let add1 x = x + 1u

            let makeSchedule id (actor: SchedulingAgent<string>) delay (words: string list) isRepeating = 
                let msg = String.Join(' ', words) 
                let ts = TimeSpan(0, 0, delay) 
                let res = actor.ScheduleAlarm(printReceiver, msg, ts, isRepeating)
                match res with
                | Success result -> schedules.Add(id, result)
                | Error msg -> eprintfn "Error: %s" msg

            let cancelAllSchedules() = 
                for alarm in schedules.Values do alarm.CancelSchedule()
                schedules.Clear()

            let updateDisplay (messages: string list) = 
                Console.SetCursorPosition(0, Console.CursorTop - 7)
                Console.WriteLine("Current - 5: {0}", "<5x prior current update>")
                Console.WriteLine("Current - 4: {0}", "<4x prior current update>")
                Console.WriteLine("Current - 3: {0}", "<3x prior current update>")
                Console.WriteLine("Current - 2: {0}", "<2x prior current update>")
                Console.WriteLine("Current - 1: {0}", "<prior current update>")
                Console.WriteLine("Current    : {0}", "<most current update here>")
                Console.Write("> ")

            printf "\nEnter command or 'help' to see available commands\n"
            let actor = SchedulingAgent() 
            printf "> "
            let rec loop cnt = 
                let input = Console.ReadLine() 
                let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                match inputList with 
                | [] -> loop cnt
                | "help"::_ -> 
                    updateDisplay showHelp
                    loop cnt
                | "once"::delay::msgs -> 
                    makeSchedule cnt actor (delay |> int32) msgs false
                    loop (add1 cnt)
                | "repeat"::delay::msgs -> 
                    makeSchedule cnt actor (delay |> int32) msgs true
                    loop (add1 cnt)
                | "stop"::id::_ -> 
                    let id' = uint32 id
                    try
                        let schedule = schedules.[id']
                        schedule.CancelSchedule()
                        schedules.Remove(id') |> ignore
                    with
                    | ex -> eprintfn "[Invalid alarm id %s] %A" id ex
                    loop cnt
                | "stop"::_ -> 
                    cancelAllSchedules()
                    loop 0u
                | "list"::_ -> 
                    for x in schedules do 
                        printfn " %i: %A" x.Key x.Value
                    loop cnt
                | "quit"::_ -> 
                    printfn "\n...quitting...cancelling all schedules..."
                    cancelAllSchedules()
                | _ -> 
                    printfn "\nUnknown command"
                    updateDisplay showHelp
                    loop cnt
            loop 0u

            printfn "done\n"

        0 // return an integer exit code
