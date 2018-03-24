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

    let private courierAgent = Kafka.KafkaCourierAgent()

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
            | Some agent -> agent.Stop() |> ignore  // TODO: log result
            | None -> eprintfn "Alarm agent servicing domain [%A] not found" domain

        let stopAgents () =
            agents |> Map.iter (fun _ agent -> agent.Stop() |> ignore)     // TODO: log result
            agents <- Map.empty

        let stopAlarm domain id =
            match agents.TryFind domain with
            | Some agent -> agent.CancelSchedule(id) |> ignore    // TODO: log result
            | None -> eprintfn "Schedule %A in Alarm servicing %s not found" id domain

        let stopAlarms () =
            agents |> Map.iter (fun _ agent -> agent.CancelSchedule() |> ignore)

        let configure forwardingHandler (ds: DataSource) =
            let delay = TimeSpan(0, 0, Math.Abs(ds.frequencyInSeconds))
            let cadence = if ds.frequencyInSeconds > 0 then Repeating else Once
            { frequency = (cadence, delay); payload = ds; handler = forwardingHandler }

        let create (agent: AlarmAgent<_>) settings =
            match agent.Schedule(settings) with
            | AlarmResult.Scheduled a -> eprintfn "Alarm set {%A} with schedule {%A}" a.agentId a.alarmId
            | AlarmResult.Error (msg, ex) -> eprintfn "Error: %s\n%A" msg ex
            | x -> eprintfn "Unexpected outcome: %A" x

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
            | AlarmResult.Error (msg, ex) -> failwithf "Error: %s\n%A" msg ex
            | e -> failwithf "Unexpected outcome: %A" e

        let getAllSchedules () =
            agents |> Map.toList
                   |> List.map (snd >> getSchedules)


    let private forwardPackage (courier: CourierAgent<_>) (waybill: Alarm<_>) =
        let package = { courierId = courier.Id; activityId = waybill.configId; 
                        causationId = waybill.alarmId; correlationId = waybill.agentId; 
                        payload = waybill.payload }
        match courier.Ship package with
        | CourierResult.Shipped delivery -> 
            eprintfn "[%s] Shipped %A" (formatTime delivery.shippedAt) delivery.payload
        | CourierResult.Stopped (at, msg) -> 
            eprintfn "[%s] Unexpected outcome: %s" (formatTime at) msg
        | CourierResult.Error (msg, ex) -> 
            eprintfn "Error: %s\n%A" msg ex

    let private forwardToCourier = forwardPackage courierAgent

    type WorkflowAgent() = 

        let agentId = Guid.NewGuid()

        let agent = Agent.Start(fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | LoadConfig (file, ch) ->
                    match configAgent.ReadConfig(file) with
                    | Configuration.ConfigRead (_, cfg) -> 
                        cfg |> Alarm.createMany forwardToCourier
                        ConfigLoaded cfg |> ch.Reply
                    | Configuration.Error (msg, ex) -> 
                        eprintfn "Error: %s\n%A" msg ex
                    | Configuration.Stopped (at, msg) -> 
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
                    configAgent.Stop() |> ignore
                    Alarm.stopAlarms ()
                    Alarm.stopAgents ()
                    courierAgent.Stop() |> ignore
                    let at = DateTimeOffset.Now
                    let msg = sprintf "Stopped workflow at %s with %i messages in queue"
                                (formatTime at) inbox.CurrentQueueLength
                    (at, msg) |> WorkflowStopped |> ch.Reply
                    return! async.Zero()
                
            }
            loop())

        member __.Id with get() = agentId

        member __.LoadConfig(file: FileInfo) =
            let buildMessage replyChannel = LoadConfig (file, replyChannel)
            agent.PostAndReply buildMessage

        member __.ListAlarms() = agent.PostAndReply (ListAlarms)

        member __.StopSchedule(domain, ?alarmId) = 
            let buildMessage replyChannel = StopAlarm (domain, alarmId, replyChannel)
            agent.PostAndReply buildMessage
        
        member __.StopWorkflow() = agent.PostAndReply (StopWorkflow)
                