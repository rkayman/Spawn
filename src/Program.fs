namespace Amber.Spawn

module App =

    open System
    open Workflow

    [<EntryPoint>]
    let main argv =
        let printUsage() = eprintfn "%s" CommandLine.usageMsg

        let controller = Workflow.WorkflowAgent()
        
        match CommandLine.parse argv with 
        | CommandLine.Help -> printUsage()
        
        | CommandLine.Errors errors -> 
            errors.PrintErrors 
            printUsage()

        | CommandLine.Options options ->
            match options.file with
            | None -> printUsage()
            | Some file ->
                match controller.LoadConfig(file) with
                | WorkflowResult.ConfigLoaded config -> 
                    eprintfn "Using configuration:\n%A" config
                | ur -> 
                    eprintfn "Unexected result: %A" ur

                let stopWorkflow _ = controller.StopWorkflow() |> eprintfn "%A"
                AppDomain.CurrentDomain.ProcessExit.Add stopWorkflow
                waitForShutdown ()
                
        0 // return an integer exit code
