namespace Amber.Spawn

[<AutoOpen>]
module ConsoleHost =
    
    open System
    open System.Threading
    open System.Threading.Tasks

    let waitForTokenShutdownAsync (token: CancellationToken) = async {
        let waitForStop = new TaskCompletionSource<obj>()
        token.Register((fun (x: obj) ->
            let tcs = x :?> TaskCompletionSource<obj>
            tcs.TrySetResult(null) |> ignore), waitForStop) |> ignore
    }

    let private attachCtrlcSigtermShutdown (cts: CancellationTokenSource) 
                                           (mre: ManualResetEventSlim) 
                                           (shutdownMessage: string option) =
        let shutdown _ =
            match cts.IsCancellationRequested with
            | true -> ()
            | false -> 
                match shutdownMessage with
                | None -> ()
                | Some msg -> printfn "%s" msg

                try
                    cts.Cancel()
                with
                    | :? ObjectDisposedException -> ()

            mre.Wait()

        do
            AppDomain.CurrentDomain.ProcessExit.Add shutdown
            Console.CancelKeyPress.Add(fun eventArgs -> do shutdown (); eventArgs.Cancel <- true)
