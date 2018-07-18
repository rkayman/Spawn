namespace Amber.Spawn

module Workflow = 

    open System
    open System.IO
    open Logary
    open Alarm
    open Configuration
    open Courier
    open Utilities
    
    type WorkflowResult<'T> = 
        | WorkflowStarted of Configuration
        | Alarms of (Guid * Alarm<'T> list) list
        | AlarmStopped of DateTimeOffset * string
        | WorkflowStopped of DateTimeOffset * string

    type WorkflowMessage<'T> = 
        | StartWorkflow of FileInfo * WorkflowResult<'T> AsyncReplyChannel
        | ListAlarms of WorkflowResult<'T> AsyncReplyChannel
        | StopAlarm of string * Guid option * WorkflowResult<'T> AsyncReplyChannel
        | StopWorkflow of WorkflowResult<'T> AsyncReplyChannel

    let private logger = Logging.getCurrentLogger ()

    let private logMessage level msg = msg |> Message.event level |> logger.logSimple

    let private formatTime (time: DateTimeOffset) = time.ToString("yyyyMMddTHH:mm:ss.fffzzz")

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
            | Some agent -> agent.Stop() 
                            |> sprintf "%A"
                            |> logMessage Info
            | None -> sprintf "Alarm agent servicing domain [%A] not found" domain
                      |> logMessage Warn

        let stopAgents () =
            let stop (a: AlarmAgent<_>) = a.Stop() |> sprintf "%A" |> logMessage Info
            agents |> Map.iter (fun _ agent -> stop agent)
            agents <- Map.empty

        let stopAlarm domain id =
            match agents.TryFind domain with
            | Some agent -> 
                agent.CancelSchedule(id) |> sprintf "%A" |> logMessage Info
            | None -> 
                sprintf "Schedule %A in Alarm servicing %s not found" id domain
                |> logMessage Warn

        let stopAlarms () =
            let stop (a: AlarmAgent<_>) = a.CancelSchedule()
                                          |> sprintf "%A" |> logMessage Info
            agents |> Map.iter (fun _ agent -> stop agent)

        let configure forwardingHandler (ds: DataSource) =
            let delay = TimeSpan(0, 0, Math.Abs(ds.frequencyInSeconds))
            let cadence = if ds.frequencyInSeconds > 0 then Repeating else Once
            { frequency = (cadence, delay); payload = ds; handler = forwardingHandler }

        let create (agent: AlarmAgent<_>) settings =
            match agent.Schedule(settings) with
            | AlarmResult.Scheduled (at, a) -> 
                sprintf "[%s] Alarm set {%A} with schedule {%A}" 
                         (formatTime at) a.agentId a.alarmId
                |> logMessage Info
            | AlarmResult.Error (at, msg, ex) -> 
                sprintf "[%s] Error: %s\n%A" (formatTime at) msg ex
                |> logMessage LogLevel.Error
            | x -> 
                sprintf "Unexpected outcome: %A" x |> logMessage Warn

        let createMany forwardingHandler (cfg: Configuration) =
            let rec loop (lst: DataSource list) =
                match lst with
                | [] -> ()
                | ds::lst' ->
                    let agent = ds.sourceUrl |> getDomainAgent
                    ds |> configure forwardingHandler |> create agent
                    loop lst'
            loop (Array.toList cfg.dataSource)

        // let createMany2 forwardingHandler fs lst = 
        //     lst |> Seq.map (fun x -> x, x |> fs.getSourceUrl |> getDomainAgent)
        //         |> Seq.map (fun (x,a) -> a, x |> configure forwardingHandler)
        //         |> Seq.iter (fun (a,x) -> create a x)

        let getSchedules (agent: AlarmAgent<_>) =
            match agent.ListSchedules() with
            | AlarmResult.Alarms (id, lst) -> id, lst
            | AlarmResult.Error (at, msg, ex) -> 
                let failmsg = sprintf "[%s] Error: %s\n%A" (formatTime at) msg ex
                failmsg |> logMessage Fatal
                failwith failmsg
            | e -> 
                let failmsg = sprintf "Unexpected outcome: %A" e
                failmsg |> logMessage Fatal
                failwith failmsg

        let getAllSchedules () =
            agents |> Map.toList
                   |> List.map (snd >> getSchedules)


    // type WorkflowAgent(courier: CourierAgent<_>, config: ConfigAgent) = 
    type WorkflowAgent(courierFactory: unit -> CourierAgent<_>, configFactory: unit -> ConfigAgent) = 

        let forwardPackageToCourier (courier: CourierAgent<_>) (waybill: Alarm<_>) =
            // TODO: implement activity id that ties back to configuration item
            let package = { messageId = Guid.NewGuid(); agentId = courier.Id; 
                            activityId = waybill.alarmId; causationId = waybill.alarmId; 
                            correlationId = waybill.agentId; payload = waybill.payload }
            match courier.Ship package with
            | CourierResult.Shipped (_, delivery) -> 
                sprintf "[%s] Shipped %A" (formatTime delivery.shippedAt) delivery.message
                |> logMessage Info
            | CourierResult.Stopped (at, msg) -> 
                sprintf "[%s] Unexpected outcome: %s" (formatTime at) msg
                |> logMessage Warn
            | CourierResult.Error (at, msg, ex) -> 
                sprintf "[%s] Error: %s\n%A" (formatTime at) msg ex
                |> logMessage LogLevel.Error

        let agentId = Guid.NewGuid()

        let agent = Agent.Start(fun inbox ->
            let courier = courierFactory ()
            let config = configFactory ()
            let forwardPackage = forwardPackageToCourier courier

            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | StartWorkflow (file, ch) -> 
                    // TODO: change this from sequential to async (include reply channel)
                    match config.ReadConfig(file) |> Async.RunSynchronously with
                    | Configuration.ConfigRead (_, cfg) -> 
                        cfg |> Alarm.createMany forwardPackage
                        WorkflowStarted cfg |> ch.Reply
                    | Configuration.Error (msg, ex) -> 
                        sprintf "Error: %s\n%A" msg ex |> logMessage LogLevel.Error
                    | Configuration.Stopped (at, msg) -> 
                        sprintf "[%s] Unexpected outcome: %s" (formatTime at) msg
                        |> logMessage Warn
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
                    config.Stop()  // TODO: log **info/error**
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

        new() = WorkflowAgent(CourierAgent, ConfigAgent)

        member __.Id with get() = agentId

        member __.Start(file: FileInfo) =
            let buildMessage replyChannel = StartWorkflow (file, replyChannel)
            agent.PostAndReply buildMessage

        member __.ListAlarms() = agent.PostAndReply (ListAlarms)

        member __.StopSchedule(domain, ?alarmId) = 
            let buildMessage replyChannel = StopAlarm (domain, alarmId, replyChannel)
            agent.PostAndReply buildMessage
        
        member __.Stop() = agent.PostAndReply (StopWorkflow)
                