namespace Amber.Spawn

module App =

    open System
    open System.Collections.Generic
    open System.IO
    open Configuration
    open Alarm
    open Courier

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
        
        let result = argv |> Array.toList |> CommandLine.parse
        match result with 
        | CommandLine.Help -> printUsage()
        
        | CommandLine.Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | CommandLine.Options _ -> 
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            let showHelp = "\nhelp\t\tShow this help message \
                            \nonce\t\tCreate a one-time timer \
                            \nrepeat\t\tCreate a repeating timer \
                            \nstop\t\tStop schedule \
                            \nlist\t\tList schedules \
                            \nconfig\t\tSet configuration \
                            \nquit\t\tQuit program\n"

            let schedules = new Dictionary<uint32, ScheduledInfo>()

            let configAgent = ConfigAgent()
            let courierAgent = Courier.Kafka.KafkaCourierAgent()
            let alarmAgent = AlarmAgent()

            let add1 x = x + 1u

            let formatTime (time: DateTimeOffset) = time.ToString("yyyyMMddTHH:mm:ss.fffzzz")

            let cancelAllSchedules() =
                for schedule in schedules.Values do schedule.CancelSchedule()
                schedules.Clear()

            let forwardPackage (courier: Courier.CourierAgent<_>) (waybill: ScheduledInfo) = 
                let package = { courierId = courier.Id; activityId = waybill.configId; 
                                causationId = waybill.scheduleId; correlationId = waybill.alarmId; 
                                payload = waybill.payload }
                match courier.Ship package with
                | CourierResult.Shipped delivery -> 
                    eprintfn "[%s] Shipped %A" (formatTime delivery.shippedAt) delivery.payload
                | CourierResult.Stopped (at, msg) -> 
                    eprintfn "[%s] Unexpected outcome: %s" (formatTime at) msg
                | CourierResult.Error (msg, ex) -> 
                    eprintfn "Error: %s\n%A" msg ex

            let forwardToCourier = forwardPackage courierAgent

            let makeAlarmInfo delay isRepeating =
                let payload = { name = String100 "Tiger #1"; 
                                sourceUrl = Uri("https://integration.tiger.pwc.com/"); 
                                protocol = Https; feed = Atom; format = Xml; recordType = WorkRecord; 
                                batchSize = 10u; maxRetries = 1us; frequencyInSeconds = 10u }
                let cadence = if isRepeating then Repeating else Once
                { frequency = (cadence, delay); payload = "Hello, World" }

            let makeSchedule id (agent: AlarmAgent) delay isRepeating = 
                let ts = TimeSpan(0, 0, delay)
                let alarm = makeAlarmInfo ts isRepeating
                match agent.ScheduleAlarm(alarm, forwardToCourier) with
                | AlarmResult.Scheduled info -> schedules.Add(id, info)
                | AlarmResult.Stopped (at, msg) -> eprintfn "[%s] Unexpected outcome: %s" (formatTime at) msg
                | AlarmResult.Error (msg, ex) -> eprintfn "Error: %s\n%A" msg ex

            let displaySchedules() = 
                for x in schedules do
                    eprintfn "%03i: Alarm = %A; Schedule = %A" x.Key x.Value.alarmId x.Value.scheduleId

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
                    makeSchedule cnt alarmAgent (delay |> int32) false
                    loop (add1 cnt)
                | "repeat"::delay::msgs -> 
                    makeSchedule cnt alarmAgent (delay |> int32) true
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
                        let result = FileInfo path |> configAgent.Configure
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
