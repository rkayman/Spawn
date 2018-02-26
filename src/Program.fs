namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.IO
    open CommandLine
    open Configuration
    open Scheduler
    open Utilities

    let readInput () =
        let rec readKey cmd =
            let ki = System.Console.ReadKey(true)
            match ki.Key with
            | ConsoleKey.Enter -> cmd
            | ConsoleKey.Escape -> cmd
            | ConsoleKey.Backspace ->
                match cmd with
                | [] -> readKey []
                | _::t ->
                    eprintf "\b \b"
                    readKey t
            | _ ->
                eprintf "%c" ki.KeyChar
                readKey (ki.KeyChar::cmd)
        let command = readKey [] |> Seq.rev |> String.Concat
        eprintfn ""
        command

    let defaultPrompt = function 
        | s when String.IsNullOrEmpty(s) -> ">" 
        | s -> s

    let readCommand prompt = 
        eprintf "%s " <| defaultPrompt prompt
        readInput ()

    [<EntryPoint>]
    let main argv =
        let printUsage() = eprintfn "%s" usageMsg
        
        let result = argv |> Array.toList |> CommandLine.parse
        match result with 
        | Help -> printUsage()
        
        | Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | Options _ -> 
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            let showHelp = "\nhelp\t\tShow this help message \
                            \nonce\t\tCreate a one-time timer \
                            \nrepeat\t\tCreate a repeating timer \
                            \nstop\t\tStop schedule \
                            \nlist\t\tList schedules \
                            \nconfig\t\tSet configuration \
                            \nquit\t\tQuit program\n"

            let schedules = new Dictionary<uint32, ScheduleResult>()

            let configActor = ConfigAgent()
            let courierActor = Courier.Kafka.KafkaCourierAgent() //CourierAgent()
            let scheduleActor = SchedulerAgent() 

            let add1 x = x + 1u

            let cancelAllSchedules() = 
                for alarm in schedules.Values do alarm.CancelSchedule()
                schedules.Clear()

            //let printReceiver msg = printfn "[%s] %s" (DateTime.Now.ToString("yyyyMMddTHH:mm:ss.fffzzz")) msg
            let forward (actor: Courier.CourierAgent<_>) msg = 
                match msg |> actor.Send with
                | Success result -> eprintfn "%A" result
                | Error msg -> eprintfn "Error: %s" msg

            let forwardToCourier = forward courierActor

            let makeSchedule id (actor: SchedulerAgent<string>) delay (words: string list) isRepeating = 
                let msg = String.Join(' ', words) 
                let ts = TimeSpan(0, 0, delay) 
                let res = actor.ScheduleAlarm(forwardToCourier, msg, ts, isRepeating)
                match res with
                | Success result -> schedules.Add(id, result)
                | Error msg -> eprintfn "Error: %s" msg

            let displaySchedules() = 
                for x in schedules do
                    eprintfn "%03i: Scheduler = %A; Timer = %A" x.Key x.Value.agentId x.Value.scheduleId

            eprintf "\nEnter command or 'help' to see available commands\n"
            let rec loop cnt = 
                let input = readCommand String.Empty
                let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                match inputList with 
                | [] -> loop cnt
                | "help"::_ -> 
                    eprintfn "%s" showHelp
                    loop cnt
                | "once"::delay::msgs -> 
                    makeSchedule cnt scheduleActor (delay |> int32) msgs false
                    loop (add1 cnt)
                | "repeat"::delay::msgs -> 
                    makeSchedule cnt scheduleActor (delay |> int32) msgs true
                    loop (add1 cnt)
                | "stop"::id::_ -> 
                    let id' = uint32 id
                    try
                        let schedule = schedules.[id']
                        schedule.CancelSchedule()
                        schedules.Remove(id') |> ignore
                        displaySchedules()
                    with
                    | ex -> eprintfn "[ERROR] Invalid alarm id %s\n%A" id ex
                    loop cnt
                | "stop"::_ -> 
                    cancelAllSchedules()
                    displaySchedules()
                    loop 0u
                | "list"::_ -> 
                    displaySchedules()
                    loop cnt
                | "config"::path::_ ->
                    match File.Exists path with
                    | false -> eprintfn "[ERROR] Unable to find [%s]" path
                    | true -> 
                        let result = FileInfo path |> configActor.Configure
                        eprintfn "%A" result                        
                    loop cnt
                | "quit"::_ -> 
                    eprintfn "\n...quitting...cancelling all schedules..."
                    cancelAllSchedules()
                | _ -> 
                    eprintfn "\n[ERROR] Unknown command"
                    eprintfn "%s" showHelp
                    loop cnt
            loop 0u

            eprintfn "done\n"

        0 // return an integer exit code
