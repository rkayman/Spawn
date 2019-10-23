namespace Spawn

open Spawn.Common
open Spawn.Configuration
open Spawn.IO.Pipes
open Spawn.Messages
open Spawn.Scheduler
open System.Reflection
open System.Threading
open System

module Daemon =
        
    [<EntryPoint>]
    let main _ =

        let logary = Logging.createConsoleTarget ()
        let log = Logging.Log(logary, "Spawn.Daemon")
        
        let processRequest (actor: AgendaActor) request =
            async {
                match request with
                | Request.Import json ->
                    let agenda = readAgenda json
                                 |> function Ok x    -> x
                                           | Error e -> raise (new FormatException(e.ToString()))
                    let! distinct = actor.Process(agenda)
                    return { total = agenda.alarms.Length; distinct = distinct } |> Imported
                
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
                    | ById value      ->
                        let idSort = fun x -> x.id.ToShortString()
                        let! xs = actor.ListId(value)
                        return xs |> list idSort
                
                | Request.Remove opt  ->
                    let! response = match opt with
                                    | ByDomain filter -> actor.RemoveDomain(filter)
                                    | ByName filter   -> actor.RemoveName(filter)
                                    | ById value      -> actor.RemoveId(value)
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
        
        let listen (log: Logging.Log) (actor: AgendaActor) (listener: SpawnServer) =
            async {
                let cancel _ = "Quitting console..." |> log.Info
                do AppDomain.CurrentDomain.ProcessExit.Add cancel
                
                let handle = processRequest actor >> Async.RunSynchronously
                let serialize (response: Response) = response.Serialize()
                let deserialize = Request.Deserialize
                
                "Spawn daemon started..." |> log.Info
                do! listener.ListenAsync(handle, serialize, deserialize)
            }

        use cts = new CancellationTokenSource()
        use agenda = new AgendaActor(logary, cts.Token)
        use server = new SpawnServer("spawn", logary, cts.Token)
        
        server |> listen log agenda |> Async.Start

        waitForShutdown()

        agenda.RemoveName(None) |> Async.RunSynchronously
                                |> sprintf "CLEANUP: Cancelled %d alarms"
                                |> log.Info
        0
