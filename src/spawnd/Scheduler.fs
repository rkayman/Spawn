namespace Spawn

open NodaTime
open System.IO
open System.Text
open System.Threading
open System
open Spawn.Clock.Alarm

 (*
 
 What does the scheduler do?
 
 Use Cases:
 0. Daemon starts (** entry point **)
 1. Scheduler starts
 2. Scheduler (aka ScheduleManager) receives a configuration request (e.g. add, update, remove)
 3. Scheduler receives a schedule request (e.g. start, stop)
 4. Scheduler receives alarm from schedule
 5. System Event interrupts Daemon/Scheduler (e.g. OS startup / shutdown; process failure)
 
 Rules:
 0.  StartDaemon
     - initialize app => start top-level dependencies: { in-memory bus; scheduler (aka ScheduleManager) }
        - scheduler initialization will subscribe to in-memory bus topic(s)
            * https://blog.mavnn.co.uk/an-in-memory-message-bus-in-100-lines-or-less/
     - let env = Map of environment settings: configFileName and envFolderLocations
     - raise DaemonStarted with let guid = new Guid; { id = guid; causation = Guid.Empty;
                                                       correlation = guid; body = env }
     - log DaemonStarted
 
 1.  OnDaemonStarted
     - find configFileName in envFolderLocations
     - read and parse config
     - raise SchedulerStarted with let guid = new Guid; { id = guid; causation = id'; correlation = correlation';
                                                          body = alarmConfigList }
     - log SchedulerStarted

 2.  OnSchedulerStarted
     - scan for, log/report, and remove duplicate configurations
     - for each config in configList
         - raise ScheduleAlarmRequested with let guid = new Guid; { id = guid; causation = id';
                                                                    correlation = correlation'; body = alarmConfig }
         - log ScheduleAlarmRequested
     
 3.  OnScheduleAlarmRequested
     - schedule (categorize/classify) alarm
     - if schedule (category) exists then add alarm to schedule.alarms collection using IgnoreAlarm
     - else create new schedule and corresponding alarms collection then add alarm using IgnoreAlarm
     - raise AlarmScheduled with let guid = new Guid; { id = guid; causation = id'; correlation = correlation';
                                                        body = alarmConfig }
     - log AlarmScheduled

 4.  OnAlarmScheduled
     - if autoStart = true then
        - change alarm state in schedule.alarms using TriggerAlarm
        - start alarm
        - raise StartAlarmRequested with let guid = new Guid; { id = guid; causation = id'; correlation = correlation';
                                                                body = ??? }
        - log StartAlarmRequested

 5.  OnStartAlarmRequested
     - change alarm state in schedule.alarms using TriggerAlarm
     - start alarm
     - raise AlarmStarted with let guid = new Guid; { id = guid; causation = ???; correlation = alarm.id }
     - log AlarmStarted
          
 6.  OnAlarmStarted
     - tbd
  
 7.  OnStopAlarmRequested
     - change alarm state in schedule.alarms using IgnoreAlarm
     - stop alarm
     - raise AlarmStopped with let guid = new Guid; { id = guid; causation = ???; correlation = alarm.id }
     - log AlarmStopped
        
 8.  OnAlarmStopped
     - tbd
     
 9.  OnScheduleTriggered
     - for each alarm in schedule.alarms
        - call TriggerAlarm (send payload to alarm.uri)
        - raise AlarmTriggered with let guid = new Guid; { id = guid; causation = schedule.id; correlation = ??? }
      
 
 *)
    
module Scheduler =
    open Messaging
    
    let private cts = new CancellationTokenSource()
    let private timeout = 10000

    // TODO: split into many agents -- one to control workflow, and one agent for each step in workflow
//    let rec private loop (agent: SchedulerMessage Agent) =
//        async {
//            try
//                let! msg = agent.TryReceive(timeout)
//                match msg with
//                | None -> return! loop agent
//                | Some msg' ->
//                    match msg' with
//                    | StartScheduler locations ->
//                        let guid = Guid.NewGuid()
//                        let evt = { id = guid; causation = guid; correlation = guid;
//                                    timestamp = Instant.FromDateTimeOffset(DateTimeOffset.Now);
//                                    name = "SchedulerStarted"; body = locations }
//                        agent.Post(SchedulerStarted evt)
//                        return! loop agent
//
//                    | SchedulerStarted evt ->
//                        let path = evt.body |> Seq.tryFind selectFirstFileFound
//                        match path with
//                        | None ->
//                            let fmtToBuf sb uri = Printf.bprintf sb "\n%A" uri; sb
//                            let xs = evt.body |> Seq.fold fmtToBuf (StringBuilder())
//                            let errmsg = sprintf "Unable to find 'spwncfg.json in: %A" xs
//                            let guid = Guid.NewGuid()
//                            let evt' = { id = guid; causation = evt.causation; correlation = evt.correlation;
//                                         timestamp = Instant.FromDateTimeOffset(DateTimeOffset.Now);
//                                         name = "SchedulerFaulted"; body = FileNotFoundException(errmsg) :> exn }
//                            agent.Post(SchedulerFaulted evt')
//                            return! loop agent
//                        
//                        | Some p ->
//                            // TODO: Read and parse json configuration file
//                            printfn "Received SchedulerStarted message"
//                            return! loop agent
//                                
//                    | SchedulerConfigured evt -> return! loop agent
//                    | ScheduleAlarm cfg -> return! loop agent
//                    | AlarmScheduled evt -> return! loop agent
//                    | StartAlarm ids -> return! loop agent
//                    | AlarmStarted evt -> return! loop agent
//                    | StopAlarm ids -> return! loop agent
//                    | AlarmStopped evt -> return! loop agent
//                    | TriggerAlarm ids -> return! loop agent
//                    | AlarmTriggered evt -> return! loop agent
//                    | SchedulerFaulted evt -> return! loop agent
//                    
//            finally
//                // TODO: convert to informational logging
//                printfn "Scheduler agent exited."
//        }
//        
//    let private agent = Agent<_>.Start(loop, cts.Token)
//    
//    type Scheduler() = class end with
//        static member StartScheduler(locations) = agent.Post()
//        static member ScheduleAlarm(config) = agent.Post()
//        static member StartAlarm(alarmIds) = agent.Post()
//        static member StopAlarm(alarmIds) = agent.Post()
//            
//        interface IDisposable with
//            member x.Dispose() =
//                cts.Cancel()
//                (agent :> IDisposable).Dispose()
        