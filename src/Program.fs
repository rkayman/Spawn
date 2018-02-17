namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.Text
    open System.Threading
    open CommandLine
    open Configuration

    type FooMessage = 
        | Once of TimeSpan * string * CancellationTokenSource AsyncReplyChannel 
        | Repeat of TimeSpan * string * CancellationTokenSource AsyncReplyChannel 
        | Stop of uint32 option * string AsyncReplyChannel 
        | List of string AsyncReplyChannel

    type Agent<'T> = MailboxProcessor<'T>

    type SchedulingAgent() = 

        let mutable dict = new Dictionary<uint32, CancellationTokenSource>()

        let printStatus msg = 
            printf "\n[%s] %s\n > " (DateTime.Now.ToString("yyyyMMdd hh:mm:ss")) msg 

        let once delay msg (cts: CancellationTokenSource) = async {
            do! Async.Sleep delay 
            if cts.IsCancellationRequested then cts.Dispose()
            else printStatus msg
        }

        let repeat delay msg (cts: CancellationTokenSource) = 
            let rec loop dueTime = async {
                do! Async.Sleep dueTime 
                if cts.IsCancellationRequested then cts.Dispose()
                else printStatus msg; return! loop dueTime
            }
            loop delay 

        let add1 x = x + 1u

        let agent = Agent.Start(fun inbox -> 
            let rec loop cnt = async {
                let! msg = inbox.Receive()
                match msg with 
                | Once (delay, message, replyChannel) -> 
                    let ms = delay.Seconds * 1000
                    printStatus <| sprintf "Raise alarm in %ims" ms
                    let cts = new CancellationTokenSource()
                    dict.Add(cnt, cts)
                    Async.StartImmediate(once ms message cts)
                    replyChannel.Reply cts
                    return! loop (add1 cnt)
                
                | Repeat (delay, message, replyChannel) -> 
                    let ms = delay.Seconds * 1000
                    printStatus <| sprintf "Raise alarm repeating every %ims" ms  
                    let cts = new CancellationTokenSource()
                    dict.Add(cnt, cts)
                    Async.StartImmediate(repeat ms message cts)
                    replyChannel.Reply cts
                    return! loop (add1 cnt)
                
                | Stop (None, replyChannel) -> 
                    let x = "Cancelling all alarms"
                    printStatus x
                    for kvp in dict do 
                        kvp.Value.Cancel() 
                    dict.Clear() 
                    replyChannel.Reply x
                    return! loop 0u

                | Stop (alarmId, replyChannel) -> 
                    let key = Option.get alarmId
                    let mutable cts: CancellationTokenSource = null
                    let success = dict.TryGetValue(key, &cts) 
                    let x = if success then
                                cts.Cancel()
                                dict.Remove(key) |> printfn "%A"
                                "Alarm cancelled"
                            else 
                                "No such alarm"
                    printStatus x
                    replyChannel.Reply x
                    return! loop cnt

                | List replyChannel -> 
                    let sb = StringBuilder(1024)
                    for x in dict do
                        Printf.bprintf sb "%i: %A\n" x.Key x.Value
                    let str = sb.ToString()
                    printStatus str
                    replyChannel.Reply str
                    return! loop cnt
                    
            }
            loop 0u)

        member __.ScheduleAlarm(ts, msg, isRepeating) = 
            let buildMessage replyChannel = 
                if isRepeating then Repeat (ts, msg, replyChannel)
                else Once (ts, msg, replyChannel) 
            agent.PostAndReply (fun rc -> rc |> buildMessage)

        member __.Stop(?alarmId) = 
            let buildMessage replyChannel = Stop (alarmId, replyChannel)
            agent.PostAndReply (fun rc -> rc |> buildMessage)

        member __.List() = 
            let buildMessage replyChannel = List (replyChannel)
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
                    let cts = actor.ScheduleAlarm(x, y, false)
                    loop ()
                | "repeat"::delay::msgs -> 
                    let x = TimeSpan(0, 0, delay |> int32) 
                    let y = String.Join(' ', msgs) 
                    let cts = actor.ScheduleAlarm(x, y, true)
                    loop ()
                | "stop"::id::_ -> 
                    let id' = id |> uint32
                    let msg = actor.Stop(id')
                    loop () 
                | "stop"::_ -> 
                    let msg = actor.Stop()
                    loop ()
                | "list"::_ -> 
                    let msg = actor.List()
                    loop ()
                | "quit"::_ -> 
                    printf "\n...quitting..."
                    let msg = actor.Stop()
                    printfn "%s" msg
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
