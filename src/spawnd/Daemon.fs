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

module Daemon =

    [<EntryPoint>]
    let main _ =
        let log = Logging.Log("Spawn.Daemon")
        
        let logRequest request =
            match request with
            | Request.Import x   -> sprintf "Import %d bytes of json" x.Length
            | Request.List x     -> sprintf "List %A" x
            | Request.Remove x   -> sprintf "Remove %A" x
            | Request.Version () -> sprintf "Show version"
            |> sprintf "REQUEST: %s"
            |> log.Info
            
        let logResponse response =
            match response with
            | Response.Imported x -> sprintf "Imported %d of %d alarms" x.distinct x.total
            | Response.Listed x   -> sprintf "Listed %d alarms" x.Length
            | Response.Removed x  -> sprintf "Removed %d alarms" x
            | Response.VersionReported x -> sprintf "Reported version = %s" x
            |> sprintf "RESPONSE: %s"
            |> log.Info
        
        let processRequest (actor: AgendaActor) (request: Request) =
            async {
                logRequest request
                match request with
                | Request.Import json ->
                    let agenda = readAgenda json
                                 |> function Ok x    -> x
                                           | Error e -> raise (new FormatException(e.ToString()))
                    let! distinct = actor.Process(agenda)
                    let response = { total = agenda.alarms.Length; distinct = distinct } |> Imported
                    logResponse response
                    return response
                
                | Request.List opt    ->
                    let list sort xs =
                        let response = xs |> Seq.sortBy sort |> Seq.toArray |> Listed
                        logResponse response
                        response
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
                                    |> Async.map Removed
                    logResponse response
                    return response
                
                | Request.Version ()  ->
                    let appVersion = Assembly.GetEntryAssembly().GetName().Version
                    let appTitle = Assembly.GetEntryAssembly().GetCustomAttributes<AssemblyTitleAttribute>()
                                   |> Seq.map (fun x -> x.Title)
                                   |> String.concat ", "
                    let response = (appTitle.ToString(), appVersion.ToString())
                                   ||> sprintf "%s\nVersion: %s"
                                    |> VersionReported
                    logResponse response
                    return response
            }
        
        let listen (log: Logging.Log) (actor: AgendaActor) (listener: SpawnServer) =
            async {
                let cancel _ = "Quitting console..." |> log.Info
                do AppDomain.CurrentDomain.ProcessExit.Add cancel
                
                let handler = processRequest actor >> Async.RunSynchronously
                let serializer (response: Response) = response.Serialize()
                let deserializer = Request.Deserialize
                
                "Spawn daemon started..." |> log.Info
                do! listener.StartAsync(handler, serializer, deserializer)
            }

        use cts = new CancellationTokenSource()
        use agenda = new AgendaActor(cts.Token)
        use server = new SpawnServer("spawn", cts.Token)
        
        server |> listen log agenda |> Async.Start

        waitForShutdown()

        agenda.RemoveName(None) |> Async.RunSynchronously
                                |> sprintf "CLEANUP: Cancelled %d alarms"
                                |> log.Info
        0
