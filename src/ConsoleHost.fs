namespace Amber.Spawn

[<AutoOpen>]
module ConsoleHost =
    // Translated to F# from example at...
    // https://stackoverflow.com/questions/41454563/how-to-write-a-linux-daemon-with-net-core?noredirect=1&lq=1
    
    open System
    open System.Threading
    open System.Threading.Tasks

    let private waitForTokenShutdownAsync (token: CancellationToken) =
        let waitForStop = new TaskCompletionSource<obj>()
        let action (x: obj) =
            let tcs = x :?> TaskCompletionSource<obj>
            tcs.TrySetResult(null) |> ignore

        token.Register(action, waitForStop) |> ignore
        Async.AwaitTask waitForStop.Task

    let private attachCtrlcSigtermShutdown (cts: CancellationTokenSource) 
                                           (mre: ManualResetEventSlim) 
                                           (shutdownMessage: string option) =
        let shutdown _ =
            match cts.IsCancellationRequested with
            | true -> ()
            | false -> 
                match shutdownMessage with
                | Some msg -> printfn "%s" msg
                | None -> ()

                try
                    cts.Cancel()
                with
                    | :? ObjectDisposedException -> ()

            mre.Wait()

        let shutdownOnCancel (eventArgs: ConsoleCancelEventArgs) =
            do shutdown ()
            eventArgs.Cancel <- true

        do
            AppDomain.CurrentDomain.ProcessExit.Add shutdown
            Console.CancelKeyPress.Add shutdownOnCancel

    let private waitWithMessageAsync (token: CancellationToken) 
                                     (shutdownMessage: string option) = async {
        match shutdownMessage with
        | Some msg -> printfn "%s" msg
        | None -> ()
        
        do! (waitForTokenShutdownAsync token |> Async.Ignore)
    }

    let waitForShutdownAsync (cancelToken: CancellationToken option) = async {
        let token = cancelToken |> Option.defaultValue CancellationToken.None
        let mre = new ManualResetEventSlim(false)
        use cts = CancellationTokenSource.CreateLinkedTokenSource(token)
        do attachCtrlcSigtermShutdown cts mre None
        do! (waitForTokenShutdownAsync cts.Token |> Async.Ignore)
        mre.Set()
    }

    let waitAsync (cancelToken: CancellationToken option) = async {
        let token = cancelToken |> Option.defaultValue CancellationToken.None
        if token.CanBeCanceled then
            do! waitWithMessageAsync token None
        else
            let mre = new ManualResetEventSlim(false)
            use cts = new CancellationTokenSource()
            do attachCtrlcSigtermShutdown cts mre (Some "Application is shutting down...")
            do! waitWithMessageAsync cts.Token (Some "Application running. Press Ctrl-C to shut down.")
            mre.Set()
    }

    let wait () = waitAsync None |> Async.RunSynchronously

    let waitForShutdown () = waitForShutdownAsync None |> Async.RunSynchronously
    