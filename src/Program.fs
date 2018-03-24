namespace Amber.Spawn

module App =

    open System
    open Workflow

    module Console =
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
        let printUsage() = eprintfn "%s" CommandLine.usageMsg

        let controller = Workflow.WorkflowAgent()
        
        match CommandLine.parse argv with 
        | CommandLine.Help -> printUsage()
        
        | CommandLine.Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | CommandLine.Options options ->
            match options.file with
            | None -> printUsage()
            | Some file ->
                match controller.LoadConfig(file) with
                | WorkflowResult.ConfigLoaded config -> 
                    eprintfn "Using configuration:\n%A" config
                | ur -> 
                    eprintfn "Unexected result: %A" ur

                let stopWorkflow _ = controller.StopWorkflow() |> eprintfn "%A"
                AppDomain.CurrentDomain.ProcessExit.Add stopWorkflow
                waitForShutdown ()
                
        // | CommandLine.Options _ ->
        //     let showHelp = "\nhelp\t\tShow this help message \
        //                     \nonce\t\tCreate a one-time timer \
        //                     \nrepeat\t\tCreate a repeating timer \
        //                     \nstop\t\tStop schedule \
        //                     \nlist\t\tList schedules \
        //                     \nconfig\t\tSet configuration \
        //                     \nquit\t\tQuit program\n"

        //     let displayAlarms() = 
        //         for x in schedules do
        //             eprintfn "%03i: Agent = %A; Alarm = %A" x.Key x.Value.alarmId x.Value.scheduleId

        //     eprintf "\nEnter command or 'help' to see available commands\n"
        //     let rec loop () =
        //         let input = Console.readCommand String.Empty
        //         let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        //         match inputList with 
        //         | [] -> loop ()
                
        //         | "help"::_ -> 
        //             eprintfn "%s" showHelp
        //             loop ()
                
        //         | "stop"::id::_ -> 
        //             let id' = uint32 id
        //             try
        //                 let schedule = schedules.[id']
        //                 schedule.CancelSchedule()
        //                 schedules.Remove(id') |> ignore
        //                 displayAlarms()
        //             with
        //             | ex -> eprintfn "[ERROR] Invalid alarm id %s\n%A" id ex
        //             loop ()
                
        //         | "stop"::_ -> 
        //             cancelAllSchedules()
        //             displayAlarms()
        //             loop ()
                
        //         | "list"::_ -> 
        //             displayAlarms()
        //             loop ()
                
        //         | "config"::path::_ ->
        //             match File.Exists path with
        //             | false -> eprintfn "[ERROR] Unable to find [%s]" path
        //             | true -> 
        //                 let result = FileInfo path |> configAgent.Configure
        //                 eprintfn "%A" result                        
        //             loop ()
                
        //         | "quit"::_ -> 
        //             eprintfn "\n...quitting...cancelling all schedules..."
        //             cancelAllSchedules()
                
        //         | _ -> 
        //             eprintfn "\n[ERROR] Unknown command"
        //             eprintfn "%s" showHelp
        //             loop ()
        //     loop ()

        //     eprintfn "done\n"

        0 // return an integer exit code
