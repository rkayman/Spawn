namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.Threading
    open CommandLine
    open Configuration

    type FooMessage<'a> = 
        | Once of ('a -> unit) * 'a * TimeSpan * CancellationTokenSource AsyncReplyChannel 
        | Repeat of ('a -> unit) * 'a * TimeSpan * CancellationTokenSource AsyncReplyChannel 

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent<'a>() = 

        let once receiver msg delay (cts: CancellationTokenSource) = async {
            do! Async.Sleep delay 
            if cts.IsCancellationRequested then 
                printfn "Cancelling alarm"
                cts.Dispose()
            else 
                msg |> receiver
        }

        let repeat receiver msg delay (cts: CancellationTokenSource) = 
            let rec loop time (cts': CancellationTokenSource) = async {
                do! Async.Sleep time 
                if cts'.IsCancellationRequested then 
                    printfn "Cancelling repeating alarm"
                    cts.Dispose()
                else 
                    msg |> receiver
                    return! loop time cts'
            }
            loop delay cts

        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with 
                | Once (receiver, message, delay, replyChannel) -> 
                    let ms = int delay.TotalMilliseconds
                    let cts = new CancellationTokenSource()
                    Async.StartImmediate(once receiver message ms cts)
                    replyChannel.Reply cts
                    return! loop ()
                
                | Repeat (receiver, message, delay, replyChannel) -> 
                    let ms = int delay.TotalMilliseconds
                    let cts = new CancellationTokenSource()
                    Async.StartImmediate(repeat receiver message ms cts)
                    replyChannel.Reply cts
                    return! loop ()
                    
            }
            loop ())

        member __.ScheduleAlarm(receiver, msg:'a, ts, isRepeating) = 
            let buildMessage replyChannel = 
                if isRepeating then Repeat (receiver, msg, ts, replyChannel)
                else Once (receiver, msg, ts, replyChannel) 
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

            let showHelp = "\nhelp\t\tShow this help message \
                            \nonce\t\tCreate a one-time timer \
                            \nrepeat\t\tCreate a repeating timer \
                            \npause\t\tPause current timer (remembers) \
                            \nstop\t\tStop current timer (resets) \
                            \nrestart\t\tRestart current timer \
                            \nquit\t\tQuit program\n"

            let printReceiver msg = printfn "%s" msg

            let alarms = new Dictionary<uint32, CancellationTokenSource>()

            let add1 x = x + 1u

            printf "\nEnter command or 'help' to see available commands\n"
            let actor = SchedulingAgent() 
            let rec loop cnt = 
                printf " > "
                let input = Console.ReadLine() 
                let inputList = input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                match inputList with 
                | [] -> loop cnt
                | "help"::_ -> 
                    printfn "%s" showHelp
                    loop cnt
                | "once"::delay::msgs -> 
                    let x = String.Join(' ', msgs) 
                    let y = TimeSpan(0, 0, delay |> int32) 
                    let cts = actor.ScheduleAlarm(printReceiver, x, y, false)
                    alarms.Add(cnt, cts)
                    loop (add1 cnt)
                | "repeat"::delay::msgs -> 
                    let x = String.Join(' ', msgs) 
                    let y = TimeSpan(0, 0, delay |> int32) 
                    let cts = actor.ScheduleAlarm(printReceiver, x, y, true)
                    alarms.Add(cnt, cts)
                    loop (add1 cnt)
                | "stop"::id::_ -> 
                    let id' = uint32 id
                    let mutable cts: CancellationTokenSource = null 
                    let success = alarms.TryGetValue(id', &cts)
                    if success then 
                        cts.Cancel()
                        alarms.Remove(id') |> ignore
                    else 
                        eprintfn "Invalid alarm id %s" id
                    loop cnt
                | "stop"::_ -> 
                    for x in alarms do 
                        x.Value.Cancel()
                    alarms.Clear()
                    loop 0u
                | "list"::_ -> 
                    for x in alarms do 
                        printfn " %i: %A" x.Key x.Value
                    loop cnt
                | "quit"::_ -> 
                    printfn "\n...quitting...cancelling all alarms..."
                    for x in alarms do 
                        x.Value.Cancel() 
                    alarms.Clear()
                | _ -> 
                    printfn "\nUnknown command"
                    printfn "%s" showHelp 
                    loop cnt
            loop 0u

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
