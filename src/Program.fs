namespace Amber.Spawn

open System
module App =

    open System
    open System.Collections.Generic
    open System.Threading
    open CommandLine
    open Configuration

    type FooMessage = 
        | Once of TimeSpan * string 
        | Repeat of TimeSpan * TimeSpan * string 
        | Stop of uint32 option
        | List 

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent() = 

        // TODO: Add dictionary to track cancellation tokens
        let mutable dict = new Dictionary<uint32, CancellationTokenSource>()

        let printStatus msg = 
            printf "\n[%s] %s\n > " (DateTime.Now.ToString("yyyyMMdd hh:mm:ss")) msg 

        let once delay msg (token: CancellationTokenSource) = async {
            do! Async.Sleep delay 
            if token.IsCancellationRequested then token.Dispose()
            else printStatus msg
        }

        let repeat initial between msg (token: CancellationTokenSource) = 
            let rec loop delay = async {
                do! Async.Sleep delay 
                if token.IsCancellationRequested then token.Dispose()
                else printStatus msg; return! loop between
            }
            loop initial 

        let add1 x = x + 1u

        let agent = Agent.Start(fun inbox -> 
            let rec loop cnt = async {
                let! msg = inbox.Receive()
                match msg with 
                | Once (delay, message) -> 
                    let ms = delay.Seconds * 1000
                    printStatus <| sprintf "Setting up alarm in %ims" ms 
                    let cts = new CancellationTokenSource()
                    dict.Add(cnt, cts)
                    Async.StartImmediate(once ms message cts)
                    return! loop (add1 cnt)
                
                | Repeat (initial, between, message) -> 
                    let ms = initial.Seconds * 1000
                    let ms' = between.Seconds * 1000 
                    printStatus <| sprintf "Setting up alarm in %ims repeating every %ims" ms ms' 
                    let cts = new CancellationTokenSource()
                    dict.Add(cnt, cts)
                    Async.StartImmediate(repeat ms ms' message cts)
                    return! loop (add1 cnt)
                
                | Stop None -> 
                    printStatus "Cancelling all timers"
                    for kvp in dict do 
                        kvp.Value.Cancel() 
                    dict.Clear() 
                    return! loop 0u

                | Stop x -> 
                    let key = Option.get x
                    let mutable cts: CancellationTokenSource = null
                    let success = dict.TryGetValue(key, &cts) 
                    if success then
                        printStatus "Cancelling timer"
                        cts.Cancel()
                        dict.Remove(key) |> printfn "%A"
                    else 
                        printStatus "No such alarm"
                    return! loop cnt

                | List -> 
                    for x in dict do
                        sprintf "%i: %A" x.Key x.Value |> printStatus
                    return! loop cnt
                    
            }
            loop 0u)

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
                | "stop"::id::_ -> 
                    let id' = id |> uint32
                    actor.SendMessage (Stop (Some id')) 
                    loop () 
                | "stop"::_ -> 
                    actor.SendMessage (Stop None)
                    loop ()
                | "list"::_ -> 
                    actor.SendMessage (List)
                    loop ()
                | "quit"::_ -> 
                    printf "\n...quitting..."
                    actor.SendMessage (Stop None)
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
