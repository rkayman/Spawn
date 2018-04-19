namespace Amber.Spawn

module App =

    open System
    open Workflow

    [<EntryPoint>]
    let main argv =
        let printUsage() = eprintfn "%s" CommandLine.usageMsg

        match CommandLine.parse argv with 
        | CommandLine.Help -> printUsage()
        
        | CommandLine.Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | CommandLine.Options options ->
            match options with
            | { config = None; host = _; topic = _ }
            | { config = _; host = None; topic = _ }
            | { config = _; host = _; topic = None } -> printUsage()

            | { config = Some file; host = Some uri; topic = Some t } ->
                let config = Configuration.ConfigAgent()
                let courier = Courier.Kafka.KafkaCourierAgent(uri.AbsoluteUri, t)
                let controller = Workflow.WorkflowAgent(courier, config)

                match controller.LoadConfig(file) with
                | WorkflowResult.ConfigLoaded config -> 
                    eprintfn "Using configuration:\n%A" config
                | ur -> 
                    eprintfn "Unexected result: %A" ur

                let stopWorkflow _ = controller.StopWorkflow() |> eprintfn "%A"
                AppDomain.CurrentDomain.ProcessExit.Add stopWorkflow
                waitForShutdown ()
                
        0 // return an integer exit code
