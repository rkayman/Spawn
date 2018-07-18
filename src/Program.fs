namespace Amber.Spawn

module App =

    let [<EntryPoint>] main _ = 
        0

    // open System
    // // open NodaTime
    // open Hopac
    // open Logary
    // open Logary.Configuration
    // open Logary.Targets
    // // open Logary.Metric
    // // open Logary.Metrics
    // open Workflow
    // open Configuration

    // [<EntryPoint>]
    // let main argv =
    //     let printUsage() = eprintfn "%s" CommandLine.usageMsg

    //     use logary = 
    //         withLogaryManager "com.pwc.tax.amber.spawnd" (
    //             Config.withTargets [
    //                 Console.create (Console.empty) "console"
    //             ] >> 
    //             // withMetrics [
    //             //     MetricConf.create (Duration.FromMilliseconds 500L) "cpu" Logary.Metrics.WinPerfCounters.cpuInformation
    //             // ] >>
    //             withRules [
    //                 Rule.createForTarget "console"
    //             ]
    //         ) |> run

    //     let logger = logary.getLogger (PointName [| "console" |])
    //     Message.eventInfo "Starting up / Reading command line..."
    //     |> logger.logSimple

    //     match CommandLine.parse argv with 
    //     | CommandLine.Help -> printUsage()
        
    //     | CommandLine.Errors errors -> 
    //         errors.PrintErrors 
    //         printUsage()

    //     | CommandLine.Options options ->
    //         match options with
    //         | { config = None; host = _; topic = _ }
    //         | { config = _; host = None; topic = _ }
    //         | { config = _; host = _; topic = None } -> printUsage()

    //         | { config = Some file; host = Some uri; topic = Some t } ->
    //             sprintf "Using host: %s\n\t with topic: %s" t uri.AbsolutePath
    //             |> Message.eventInfo |> logger.logSimple 
    //             let config = Configuration.ConfigAgent
    //             let courier = fun () -> Courier.Kafka.KafkaCourierAgent<DataSource>(uri.AbsoluteUri, t)
    //             let workflow = Workflow.WorkflowAgent(courier, config)

    //             match workflow.Start(file) with
    //             | WorkflowResult.WorkflowStarted config -> 
    //                 sprintf "Using configuration: %A" config
    //                 |> Message.eventInfo |> logger.logSimple
    //             | ur -> 
    //                 sprintf "Unexpected result: %A" ur
    //                 |> Message.eventError |> logger.logSimple

    //             let stopWorkflow _ = workflow.Stop() |> eprintfn "%A"
    //             AppDomain.CurrentDomain.ProcessExit.Add stopWorkflow
    //             waitForShutdown ()
                
    //     0 // return an integer exit code
