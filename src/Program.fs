namespace Amber.Spawn

module App =

    open System
    open System.Threading
    open CommandLine
    open Configuration

    type FooMessage = 
        | Once of TimeSpan * string 
        | Repeat of TimeSpan * string 
        | Pause 
        | Stop 
        | Restart 

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent() = 

        let mutable bar = String.Empty

        let cts = new CancellationTokenSource() 

        let printStatus msg = 
            printfn "[%s] %s" (DateTime.Now.ToString("yyyymmdd hh:mm:ss")) msg 

        let agent = Agent.Start(fun inbox -> 
            let rec loop delay remaining reduceBy repeat = async {
                let! msg = inbox.TryReceive(reduceBy)
                match msg with 
                | None when remaining > 0 -> 
                    return! loop delay (remaining - reduceBy) reduceBy repeat
                | None when delay > 0 -> 
                    printStatus bar 
                    if repeat then return! loop delay delay reduceBy repeat 
                    else return! loop 0 0 Timeout.Infinite false 
                | None -> 
                    return! loop delay remaining reduceBy repeat  
                | Some x -> 
                    match x with 
                    | Once (initialDelay, resource) -> 
                        let ms = initialDelay.Seconds * 1000
                        let x = sprintf "Setting up one-time alarm in %ims" ms 
                        printStatus x
                        bar <- resource
                        return! loop ms ms 1000 false
                    | Repeat (initialDelay, resource) -> 
                        let ms = initialDelay.Seconds * 1000
                        let x = sprintf "Setting up repeating alarm in %ims" ms 
                        printStatus x
                        bar <- resource
                        return! loop ms ms 1000 true 
                    | Pause -> 
                        printStatus "Pausing timer" 
                        return! loop delay remaining Timeout.Infinite repeat 
                    | Stop -> 
                        printStatus "Stopping timer" 
                        return! loop delay delay Timeout.Infinite repeat 
                    | Restart -> 
                        printStatus "Resuming timer" 
                        return! loop delay remaining 1000 repeat 
            }
            loop 0 0 Timeout.Infinite false)

        member __.SendMessage (msg: FooMessage) = agent.Post msg 


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

            let showHelp = "\nhelp\t\tShow this help message \
                            \nonce\t\tCreate a one-time timer \
                            \nrepeat\t\tCreate a repeating timer \
                            \npause\t\tPause current timer (remembers) \
                            \nstop\t\tStop current timer (resets) \
                            \nrestart\t\tRestart current timer \
                            \nquit\t\tQuit program\n"

            printf "\nEnter command or 'help' to see available commands\n"
            let actor = SchedulingAgent() 
            let rec loop () = 
                printf " > "
                let input = Console.ReadLine() 
                let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                match inputList with 
                | [] -> loop ()
                | "help"::_ -> 
                    printfn "%s" showHelp
                    loop ()
                | "once"::delay::msgs -> 
                    let x = TimeSpan(0, 0, delay |> int32) 
                    let y = String.Join(' ', msgs) 
                    actor.SendMessage (Once (x, y))
                    loop ()
                | "repeat"::delay::msgs -> 
                    let x = TimeSpan(0, 0, delay |> int32) 
                    let y = String.Join(' ', msgs) 
                    actor.SendMessage (Repeat (x, y))
                    loop ()
                | "pause"::_ -> 
                    actor.SendMessage (Pause)
                    loop () 
                | "stop"::_ -> 
                    actor.SendMessage (Stop) 
                    loop () 
                | "restart"::_ -> 
                    actor.SendMessage (Restart) 
                    loop () 
                | "quit"::_ -> 
                    printf "\n...quitting..."
                | _ -> 
                    printfn "\nUnknown command"
                    printfn "%s" showHelp 
                    loop ()
            loop ()

            // printf "\n(Press 'esc' or 'q' to quit) > "
            // let rec loop () = 
            //     let input = Console.ReadKey(true)
            //     match input.Key with 
            //     | ConsoleKey.Q | ConsoleKey.Escape -> 
            //         // TODO: cancel all actors (wait for them to finish)
            //         printf "\n...quitting..."
            //     | _ -> 
            //         loop ()
            // loop ()
            printfn "done\n"

        0 // return an integer exit code
