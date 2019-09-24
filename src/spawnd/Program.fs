namespace Spawn

open Spawn.Common
open Spawn.Configuration
open Spawn.IO
open Spawn.Messages
open Spawn.Scheduler
open System.Reflection
open System.Threading
open System

module Program =

    [<EntryPoint>]
    let main _ =
        
        use cts = new CancellationTokenSource()

        let logger (inbox: Agent<_>) =
            async {
                while true do
                    let! msg = inbox.Receive()
                    msg |> string |> printfn "%s"
            }
        use log = Agent<string>.Start(logger, cts.Token)
        
        let processRequest (actor: AgendaActor) (request: Request) =
            async {
                match request with
                | Request.Import json ->
                    let agenda = readAgenda json
                                 |> function Ok x -> x | Error e -> raise (new FormatException(e.ToString()))
                    let! distinct = actor.Process(agenda)
                    return Imported { total = agenda.alarms.Length; distinct = distinct }
                
                | Request.List opt    ->
                    let list sort xs = xs |> Seq.sortBy sort |> Seq.toArray |> Listed
                    match opt with
                    | ByDomain filter ->
                        let domainSort = fun x -> x.domain, x.name
                        let! xs = actor.ListDomain(filter)
                        return xs |> list domainSort
                    | ByName filter   ->
                        let nameSort = fun x -> x.name
                        let! xs = actor.ListName(filter)
                        return xs |> list nameSort
                
                | Request.Remove opt  ->
                    let! response = match opt with
                                    | ByDomain filter -> actor.RemoveDomain(filter)
                                    | ByName filter   -> actor.RemoveName(filter)
                    return Removed response
                
                | Request.Version ()  ->
                    let appVersion = Assembly.GetEntryAssembly().GetName().Version
                    let appTitle = Assembly.GetEntryAssembly().GetCustomAttributes<AssemblyTitleAttribute>()
                                   |> Seq.map (fun x -> x.Title)
                                   |> String.concat ", "
                    return (appTitle.ToString(), appVersion.ToString())
                           ||> sprintf "%s\nVersion: %s"
                           |> VersionReported
            }
        
        let console (logger: Agent<string>) (token: CancellationTokenSource) =
            async {
                use spawn = new AgendaActor(logger, token.Token)
                
                let cancel _ =
                    printfn "\nQuitting console..."
                    spawn.RemoveName(None) |> Async.RunSynchronously
                                           |> sprintf "Cancelled %d alarms"
                                           |> log.Post
                do AppDomain.CurrentDomain.ProcessExit.Add cancel
                
                printfn "Spawn daemon started..."
                while not token.IsCancellationRequested do
                    use server = new SpawnServer("spawn")                    
                    let! message = server.GetRequestAsync(token.Token)
                    match message with
                    | Error e    -> sprintf "%A" e |> log.Post
                    | Ok request ->
                        //request |> sprintf "Received: %A" |> log.Post
                        let! response = request |> processRequest spawn
                        server.SendResponseAsync(response, token.Token) |> Async.RunSynchronously |> ignore
                        //response |> sprintf "Sent: %A" |> log.Post
            }

        console log cts |> Async.Start

        waitForShutdown()
        0

(*
    let main argv =

        let logary =
          Config.create "svc" "localhost"
          |> Config.target (LiterateConsole.create LiterateConsole.empty "nice console")
          |> Config.ilogger (ILogger.LiterateConsole Info)
          |> Config.build
          |> run
        let logger = logary.getLogger "spawnd"
        logary.flushPending() |> run

        let logMessage msgFactory steps result =
            sprintf "Step %2d = %d" steps result |> Message.eventX |> msgFactory
            logary.flushPending() |> run

        let logInfo = logMessage logger.info
        let logDebug = logMessage logger.debug

        use cts = new CancellationTokenSource()
        printfn "Press Ctrl-C to exit..."

        let per str =
            let toDigit ch = int64 ch - int64 '0'
            let rec loop str cnt =
                let total = str |> Seq.map toDigit
                                |> Seq.reduce (*)
                logInfo cnt total
                if total > 9L then loop (string total) (cnt+1)
                else total, cnt
            if String.length str = 0 then 0L, 0
            else loop str 1

        let run () = async {
            let cancel _ = printfn "Quitting inner loop..."
            do AppDomain.CurrentDomain.ProcessExit.Add cancel

            while not cts.IsCancellationRequested do
                printf "\nEnter number: "
                Console.ReadLine() |> per |> ignore
                do! Async.Sleep 100
        }

        run () |> Async.Start

        waitForShutdown ()

        printfn "\nDone."
        0
*)
