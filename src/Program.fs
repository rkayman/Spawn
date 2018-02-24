namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.IO
    open System.Threading
    open CommandLine
    open Configuration
    open Scheduler
    open Utilities

    type Agent<'T> = MailboxProcessor<'T>

    type ConfigAgentResponse = 
        | Watching of Guid * string * CancellationTokenSource
        | Configured of Guid * string
        | Ignored of Guid * string

    type private ConfigAgentMessage = 
        | WatchFile of FileInfo * ConfigAgentResponse ResultMessage AsyncReplyChannel
        // | WatchFolder of DirectoryInfo * ConfigurationManagerResponse ResultMessage AsyncReplyChannel
        | Configure of Configuration * ConfigAgentResponse ResultMessage AsyncReplyChannel

    type ConfigAgent(cancelToken: CancellationToken) = 

        let agentId = Guid.NewGuid()

        let watchFile (fi: FileInfo) 
                      (ch: ConfigAgentResponse ResultMessage AsyncReplyChannel) = 
            let cts = new CancellationTokenSource()
            let response = sprintf "Watching: %s" fi.FullName
            printfn "%s" response
            Success (Watching (agentId, response, cts)) |> ch.Reply

        let configure (cfg: Configuration) 
                      (ch: ConfigAgentResponse ResultMessage AsyncReplyChannel) = 
            let response = sprintf "%A" cfg
            printfn "%s" response
            Success (Configured (agentId, response)) |> ch.Reply

        let processor (inbox: Agent<_>) = 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | WatchFile (fi, ch) -> watchFile fi ch
                | Configure (cfg, ch) -> configure cfg ch

                return! loop ()
            }
            loop ()

        let agent = Agent.Start(processor, cancelToken)

        member __.ScheduleWith(cfg: Configuration) =
            let buildMessage chan = Configure (cfg, chan)
            agent.PostAndReply (fun ch -> ch |> buildMessage)

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
        
        let result = argv |> Array.toList |> parse
        match result with 
        | Help -> printUsage()
        
        | Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | Options options -> 
            eprintfn "\nUsing configuration found in %A...\n" options.file.Value.FullName 
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            let config = Configuration.ReadConfig <| Option.get options.file
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
                let input = readCommand String.Empty
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
