namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open CommandLine
    open Configuration
    open Scheduler
    open Utilities

    [<EntryPoint>]
    let main argv =
        let args = Array.toList argv
        let result = parse args
        let printUsage() = usageMsg |> eprintfn "%s" 

        match result with 
        | Help -> printUsage()
        
        | Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | Options options -> 
            eprintfn "\nUsing configuration found in %A...\n" options.file.Value.FullName 
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            let config = getConfiguration <| Option.get options.file
            printfn "%A" config

            let showHelp = "\nhelp\t\tShow this help message \
                            \nonce\t\tCreate a one-time timer \
                            \nrepeat\t\tCreate a repeating timer \
                            \nstop\t\tStop schedule \
                            \nquit\t\tQuit program\n"

            let schedules = new Dictionary<uint32, ScheduleResult>()

            let add1 x = x + 1u

            let cancelAllSchedules() = 
                for alarm in schedules.Values do alarm.CancelSchedule()
                schedules.Clear()

            let printReceiver msg = printfn "[%s] %s" (DateTime.Now.ToString("yyyyMMddTHH:mm:ss.fffzzz")) msg

            let makeSchedule id (actor: SchedulingAgent<string>) delay (words: string list) isRepeating = 
                let msg = String.Join(' ', words) 
                let ts = TimeSpan(0, 0, delay) 
                let res = actor.ScheduleAlarm(printReceiver, msg, ts, isRepeating)
                match res with
                | Success result -> schedules.Add(id, result)
                | Error msg -> eprintfn "Error: %s" msg

            let displaySchedules() = 
                for x in schedules do
                    eprintfn "%03i: Agent=%A; Schedule=%A" x.Key x.Value.agentId x.Value.scheduleId

            eprintf "\nEnter command or 'help' to see available commands\n"
            let actor = SchedulingAgent() 
            let rec loop cnt = 
                eprintf "\n> "
                let input = Console.ReadLine() 
                let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                match inputList with 
                | [] -> loop cnt
                | "help"::_ -> 
                    eprintfn "%s" showHelp
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
                        displaySchedules()
                    with
                    | ex -> eprintfn "[Invalid alarm id %s] %A" id ex
                    loop cnt
                | "stop"::_ -> 
                    cancelAllSchedules()
                    displaySchedules()
                    loop 0u
                | "list"::_ -> 
                    displaySchedules()
                    loop cnt
                | "quit"::_ -> 
                    eprintfn "\n...quitting...cancelling all schedules..."
                    cancelAllSchedules()
                | _ -> 
                    eprintfn "\nUnknown command"
                    eprintfn "%s" showHelp
                    loop cnt
            loop 0u

            eprintfn "done\n"

        0 // return an integer exit code
