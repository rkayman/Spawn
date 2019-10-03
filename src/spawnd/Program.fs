namespace Spawn

open Spawn.Common
open Spawn.Configuration
open Spawn.IO.Pipes
open Spawn.Messages
open Spawn.Scheduler
open FsToolkit.ErrorHandling
open System.Reflection
open System.Threading
open System

module Program =

    [<EntryPoint>]
    let main _ =

        printfn "Spawn daemon starting..."
        
        let processRequest (actor: AgendaActor) (request: Request) =
            async {
                match request with
                | Request.Import json ->
                    let agenda = readAgenda json
                                 |> function Ok x    -> x
                                           | Error e -> raise (new FormatException(e.ToString()))
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
        
        let listen (logger: Agent<string>) (actor: AgendaActor) (listener: SpawnServer) =
            async {
                let cancel _ =
                    printfn "\nQuitting console..."
                    actor.RemoveName(None) |> Async.RunSynchronously
                                           |> sprintf "Canceled %d alarms"
                                           |> logger.Post
                do AppDomain.CurrentDomain.ProcessExit.Add cancel
                
                let handler = processRequest actor >> Async.RunSynchronously
                let serializer (response: Response) = response.Serialize()
                let deserializer = Request.Deserialize
                
                printfn "Spawn daemon started..."
                do! listener.StartAsync(handler, serializer, deserializer)
            }
        
        let logMessage (inbox: Agent<_>) =
            async {
                while true do
                    let! msg = inbox.Receive()
                    msg |> string |> printfn "%s"
            }
        use logger = Agent<string>.Start(logMessage)

        use cts = new CancellationTokenSource()
        use agenda = new AgendaActor(logger, cts.Token)
        use server = new SpawnServer("spawn", cts.Token)
        
        server |> listen logger agenda |> Async.Start

        waitForShutdown()
        0
