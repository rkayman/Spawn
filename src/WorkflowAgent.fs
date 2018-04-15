namespace Amber.Spawn

module Workflow = 

    open System
    open System.IO
    open Alarm
    open Configuration
    open Courier
    open Utilities
    
    type WorkflowResult<'T> = 
        | ConfigLoaded of Configuration
        | Alarms of (Guid * Alarm<'T> list) list
        | AlarmStopped of DateTimeOffset * string
        | WorkflowStopped of DateTimeOffset * string

    type WorkflowMessage<'T> = 
        | LoadConfig of FileInfo * WorkflowResult<'T> AsyncReplyChannel
        | ListAlarms of WorkflowResult<'T> AsyncReplyChannel
        | StopAlarm of string * Guid option * WorkflowResult<'T> AsyncReplyChannel
        | StopWorkflow of WorkflowResult<'T> AsyncReplyChannel


    let private formatTime (time: DateTimeOffset) = time.ToString("yyyyMMddTHH:mm:ss.fffzzz")

    let private configAgent = ConfigAgent()

    module private Alarm = 

        let mutable agents: Map<string, AlarmAgent<DataSource>> = Map.empty

        let parseDomain (take: int option) (url: Uri) = 
            match take with
            | None -> url.DnsSafeHost.ToLowerInvariant()
            | Some x -> url.DnsSafeHost.ToLowerInvariant().Split(".") 
                        |> Array.rev |> Array.take x |> Array.rev |> (fun arr -> (".", arr)) |> String.Join

        let makeAgent domain =
            let agent = AlarmAgent<DataSource>()
            agents <- agents.Add(domain, agent)
            agent

        let agentFactory domain =
            match agents.TryFind domain with
            | Some alarm -> alarm
            | None -> makeAgent domain

        let getAgent domainLevel url = url |> parseDomain domainLevel |> agentFactory

        let getDomainAgent = getAgent (Some 2)

        //let getSubdomainAgent = getAgent (Some 3)

        let stopAgent domain =
            match agents.TryFind domain with
            | Some agent -> agent.Stop() |> eprintfn "%A"   // TODO: log **info/error**
            | None -> eprintfn "Alarm agent servicing domain [%A] not found" domain

        let stopAgents () =
            // log **info**
            agents |> Map.iter (fun _ agent -> agent.Stop() |> eprintfn "%A")
            agents <- Map.empty

        let stopAlarm domain id =
            match agents.TryFind domain with
            | Some agent -> 
                // TODO: log **info/error**
                agent.CancelSchedule(id) |> eprintfn "%A"
            | None -> 
                // TODO: log **warning**
                eprintfn "Schedule %A in Alarm servicing %s not found" id domain

        let stopAlarms () =
            // TODO: log **info**
            agents |> Map.iter (fun _ agent -> agent.CancelSchedule() |> eprintfn "%A")

        let configure forwardingHandler (ds: DataSource) =
            let delay = TimeSpan(0, 0, Math.Abs(ds.frequencyInSeconds))
            let cadence = if ds.frequencyInSeconds > 0 then Repeating else Once
            { frequency = (cadence, delay); payload = ds; handler = forwardingHandler }

        let create (agent: AlarmAgent<_>) settings =
            match agent.Schedule(settings) with
            | AlarmResult.Scheduled (at, a) -> 
                // TODO: log **info**
                eprintfn "[%s] Alarm set {%A} with schedule {%A}" 
                         (formatTime at) a.agentId a.alarmId
            | AlarmResult.Error (at, msg, ex) -> 
                // TODO: log **error**
                eprintfn "[%s] Error: %s\n%A" (formatTime at) msg ex
            | x -> 
                // TODO: log **warning**
                eprintfn "Unexpected outcome: %A" x

        let createMany forwardingHandler (cfg: Configuration) =
            let rec loop (lst: DataSource list) =
                match lst with
                | [] -> ()
                | ds::lst' ->
                    let agent = ds.sourceUrl |> getDomainAgent
                    ds |> configure forwardingHandler |> create agent
                    loop lst'
            loop (Array.toList cfg.dataSource)

        let getSchedules (agent: AlarmAgent<_>) =
            match agent.ListSchedules() with
            | AlarmResult.Alarms (id, lst) -> id, lst
            | AlarmResult.Error (at, msg, ex) -> 
                // TODO: log **error**
                failwithf "[%s] Error: %s\n%A" (formatTime at) msg ex
            | e -> 
                // TODO: log **warning**
                failwithf "Unexpected outcome: %A" e

        let getAllSchedules () =
            agents |> Map.toList
                   |> List.map (snd >> getSchedules)


    type WorkflowAgent(courier: CourierAgent<_>) = 

        let forwardPackage (waybill: Alarm<_>) =
            // TODO: implement activity id that ties back to configuration item
            let package = { messageId = Guid.NewGuid(); agentId = courier.Id; 
                            activityId = waybill.alarmId; causationId = waybill.alarmId; 
                            correlationId = waybill.agentId; payload = waybill.payload }
            match courier.Ship package with
            | CourierResult.Shipped (_, delivery) -> 
                // TODO: log **info**
                eprintfn "[%s] Shipped %A" (formatTime delivery.shippedAt) delivery.message
            | CourierResult.Stopped (at, msg) -> 
                // TODO: log **warning**
                eprintfn "[%s] Unexpected outcome: %s" (formatTime at) msg
            | CourierResult.Error (at, msg, ex) -> 
                // TODO: log **error**
                eprintfn "[%s] Error: %s\n%A" (formatTime at) msg ex

        let agentId = Guid.NewGuid()

        let agent = Agent.Start(fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | LoadConfig (file, ch) ->
                    match configAgent.ReadConfig(file) with
                    | Configuration.ConfigRead (_, cfg) -> 
                        cfg |> Alarm.createMany forwardPackage
                        ConfigLoaded cfg |> ch.Reply
                    | Configuration.Error (msg, ex) -> 
                        // TODO: log **error**
                        eprintfn "Error: %s\n%A" msg ex
                    | Configuration.Stopped (at, msg) -> 
                        // TODO: log **warning**
                        eprintfn "[%s] Unexpected outcome: %s" (formatTime at) msg
                    return! loop()

                | ListAlarms ch ->
                    Alarm.getAllSchedules () |> Alarms |> ch.Reply
                    return! loop ()

                | StopAlarm (domain, id, ch) ->
                    let msg = match id with
                              | Some guid -> 
                                    Alarm.stopAlarm domain guid
                                    sprintf "Stopped schedule %A serviced in alarm with domain %s." guid domain
                              | None -> Alarm.stopAlarms (); "All alarm schedules stopped"
                    (DateTimeOffset.Now, msg) |> AlarmStopped |> ch.Reply
                    return! loop()
                
                | StopWorkflow ch ->
                    configAgent.Stop()  // TODO: log **info/error**
                        |> eprintfn "%A"
                    Alarm.stopAlarms ()
                    Alarm.stopAgents ()
                    courier.Stop()  // TODO: log **info/error**
                        |> eprintfn "%A"
                    let at = DateTimeOffset.Now
                    let msg = sprintf "Stopped workflow at %s with %i messages in queue"
                                (formatTime at) inbox.CurrentQueueLength
                    (at, msg) |> WorkflowStopped |> ch.Reply
                    return! async.Zero()
                
            }
            loop())

        new() = WorkflowAgent(CourierAgent())

        member __.Id with get() = agentId

        member __.LoadConfig(file: FileInfo) =
            let buildMessage replyChannel = LoadConfig (file, replyChannel)
            agent.PostAndReply buildMessage

        member __.ListAlarms() = agent.PostAndReply (ListAlarms)

        member __.StopSchedule(domain, ?alarmId) = 
            let buildMessage replyChannel = StopAlarm (domain, alarmId, replyChannel)
            agent.PostAndReply buildMessage
        
        member __.StopWorkflow() = agent.PostAndReply (StopWorkflow)
                