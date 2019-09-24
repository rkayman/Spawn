namespace Spawn

open CommandLine
open Common
open IO
open Messages
open NodaTime
open System.Globalization
open System.Reflection
open System.Threading
open System

module CLI =

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

        do Console.CancelKeyPress.Add shutdownOnCancel

    [<EntryPoint>]
    let main args =
    
        let command = args |> Array.toList |> CommandLine.parseCommandLine

        use cts = new CancellationTokenSource()
        use mre = new ManualResetEventSlim(false)
        do attachCtrlcSigtermShutdown cts mre (Some "Cancelling operation...")
        mre.Set()

        use clientPipe = new SpawnClient(".", "spawn")
        
        let limit len (str: string) =
            match str with
            | s when s.Length <= len -> s
            | s -> s.Substring(0, len - 1) |> sprintf "%s…"
            
        let formatCount n =
            let scaleIds = [ " "; "K"; "M"; "B"; "T"]
            let rec scale x cnt =
                if x < 1000L then (x, cnt)
                else scale (x / 1000L) (cnt + 1)
            let v, s = scale n 0
            sprintf "%s %s" (v.ToString("#,##0")) scaleIds.[s]
            
        let formatInstant (i: Instant option) =
            i |> Option.map (fun x -> x.ToString("yyyy-MM-ddTHH:mm:ss", CultureInfo.CurrentCulture))
              |> Option.defaultValue "None"
            
        let handleResponse response =
            match response with
            | Imported info ->
                printfn "Processed %d alarms total. There were %d unique alarms" info.total info.distinct
                
            | Removed qty ->
                printfn "Removed %d alarms" qty
                
            | VersionReported ver ->
                printfn "%s" ver
                
            | Listed alarms ->
                let formattedPrint = printfn "%-12s │ %-12s │ %-30s │ %7s │ %-19s"
                formattedPrint "ALARM ID" "DOMAIN" "NAME" "COUNT" "LAST TRIGGERED"
                alarms |> Seq.iter (fun x -> formattedPrint (x.id.ToShortString())
                                                            (limit 12 x.domain)
                                                            (limit 30 x.name)
                                                            (formatCount x.count)
                                                            (formatInstant x.last))
                alarms |> Seq.length |> printfn "%d items"
            
        let appVersion = Assembly.GetEntryAssembly().GetName().Version
        let appTitle = Assembly.GetEntryAssembly().GetCustomAttributes<AssemblyTitleAttribute>()
                       |> Seq.map (fun x -> x.Title)
                       |> String.concat ", "
        
        match command |> toRequest cts.Token with
        | Choice1Of3 request ->
            async {
                let! _ = clientPipe.SendRequestAsync(request, cts.Token)
                let! response = clientPipe.GetResponseAsync(cts.Token)
                return response
            } |> Async.RunSynchronously
            |> Result.map handleResponse
            |> Result.mapError (eprintfn "%s")
            |> ignore
        
        | Choice2Of3 help ->
            help
            |> List.append [ appTitle.ToString(); appVersion.ToString() |> sprintf "Version: %s" ]
            |> Seq.iter (printfn "    %s")
            
        | Choice3Of3 error ->
            eprintfn "[ERROR] %s" error

        0
        