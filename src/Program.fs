namespace Amber.Spawn

module App =

    open System
    open System.Threading
    open CommandLine
    open Configuration

    type FooMessage = 
        | Once of TimeSpan * string 
        | Repeat of TimeSpan * TimeSpan * string 
        | Stop 

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent() = 

        let mutable (cts: CancellationTokenSource) = null 

        let printStatus msg = 
            printf "\n[%s] %s\n > " (DateTime.Now.ToString("yyyymmdd hh:mm:ss")) msg 

        let once delay msg = async {
            do! Async.Sleep delay 
            if isNull cts |> not && cts.IsCancellationRequested 
            then cts.Dispose(); cts <- null
            else printStatus msg
        }

        let repeat initial between msg = 
            let rec loop delay = async {
                do! Async.Sleep delay 
                if isNull cts |> not && cts.IsCancellationRequested 
                then cts.Dispose(); cts <- null
                else printStatus msg; return! loop between
            }
            loop initial 

        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with 
                | Once (delay, message) -> 
                    let ms = delay.Seconds * 1000
                    printStatus <| sprintf "Setting up alarm in %ims" ms 
                    cts <- if isNull cts then new CancellationTokenSource()
                           else
                               cts.Cancel() 
                               new CancellationTokenSource()
                    Async.StartImmediate(once ms message)
                    return! loop ()
                
                | Repeat (initial, between, message) -> 
                    let ms = initial.Seconds * 1000
                    let ms' = between.Seconds * 1000 
                    printStatus <| sprintf "Setting up alarm in %ims repeating every %ims" ms ms' 
                    cts <- if isNull cts then new CancellationTokenSource()
                           else
                               cts.Cancel() 
                               new CancellationTokenSource()
                    Async.StartImmediate(repeat ms ms' message)
                    return! loop ()

                | Stop -> 
                    if isNull cts then () 
                    else 
                        printStatus "Cancelling timer"
                        cts.Cancel() 
                    return! loop ()
            }
            loop ())

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
                | "repeat"::initial::between::msgs -> 
                    let x = TimeSpan(0, 0, initial |> int32) 
                    let y = TimeSpan(0, 0, between |> int32) 
                    let z = String.Join(' ', msgs) 
                    actor.SendMessage (Repeat (x, y, z))
                    loop ()
                | "stop"::_ -> 
                    actor.SendMessage (Stop) 
                    loop () 
                | "quit"::_ -> 
                    printf "\n...quitting..."
                    actor.SendMessage (Stop)
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
