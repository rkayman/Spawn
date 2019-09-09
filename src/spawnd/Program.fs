namespace Spawn

open Scheduler
open Spawn.IO.Messages
open Spawn.IO.Configuration
open System
open System.IO
open System.Threading

module Program =

    module CommandLine =
        
        let private badCommand extraMessage = eprintfn "Bad command %s" extraMessage; Help

        let inline private toUpper (s: string) = s.ToUpper()
        
        let inline private equals (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)

        let inline private stripQuotes (s: string) = s.Replace("\"", String.Empty, StringComparison.InvariantCulture)

        let private parseHelp _ = Help

        let private parseImport args =
            match args with
            | [ x ] when File.Exists(x) -> FileInfo(x) |> Import
            | _ -> badCommand "or missing file"

        let private parseList args =
            match args with
            | [] -> List (ByDomain None)
            | x :: [] when x |> equals "ByDomain" -> ByDomain None |> List
            | x :: [] when x |> equals "ByName"   -> ByName None   |> List
            | x :: y :: [] when x |> equals "ByDomain" -> Some y |> ByDomain |> List
            | x :: y :: [] when x |> equals "ByName"   -> Some y |> ByName   |> List
            | _ -> badCommand ""

        let private parseRemove args =
            match args with
            | [] -> badCommand ""
            | x :: y :: [] when x |> equals "ByDomain" -> stripQuotes y |> Some |> ByDomain |> Remove
            | x :: y :: [] when x |> equals "ByName"   -> stripQuotes y |> Some |> ByName   |> Remove
            | _ -> badCommand ""

        let private commandMap =
            [ ("IMPORT", parseImport); ("LIST", parseList); ("REMOVE", parseRemove);
              ("HELP", parseHelp); ("/h", parseHelp); ("-h", parseHelp); ("-?", parseHelp); ("/?", parseHelp) ]
            |> Map.ofSeq

        let private isCommand str = str |> toUpper |> commandMap.ContainsKey

        let parseCommandLine args =
            match args with
            | [] -> badCommand ""
            | x :: xs when isCommand x ->
                let invApply x f = f x
                let parser = invApply xs
                commandMap |> Map.find (toUpper x) |> parser
            | _ -> badCommand ""

    [<EntryPoint>]
    let main _ =
        
        let makeArg (cs: char list) (isQuoted: bool) (xs: string list) =
            if isQuoted then failwith "Illegal input. Missing closing quote."
            elif List.length cs = 0 then cs, isQuoted, xs
            else [], false, List.append xs [String.Concat(cs)]
        
        let makeArgs (str: string) =
            let folder (s: char list * bool * string list) t =
                let cs, isQuoted, xs = s
                let ws = [' '; '\n'; '\r']
                match isQuoted, t with
                | true, '\"' -> makeArg cs false xs
                | false, '\"' ->
                    if List.length cs = 0 then [], true, xs
                    else failwith "Illegal input. Unexpected double quote character."
                | false, ch when Seq.contains ch ws ->
                    if List.length cs = 0 then [], false, xs
                    else makeArg cs isQuoted xs
                | _, ch -> List.append cs [ch], isQuoted, xs
                
            str.ToCharArray() |> Seq.fold folder ([], false, [])
                              |> fun (x, y, z) -> makeArg x y z
                              |> fun (_, _, z) -> z

        use cts = new CancellationTokenSource()

        let console (token: CancellationTokenSource) = async {
            use actor = new AgendaActor(token.Token)
            
            let cancel _ =
                printfn "Quitting console... IsCancellationRequested? %b" token.IsCancellationRequested
                actor.Dispose()
                Async.Sleep(2000) |> Async.RunSynchronously
            do AppDomain.CurrentDomain.ProcessExit.Add cancel
            
            let limit len (str: string) =
                match str with
                | s when s.Length <= len -> s
                | s -> s.Substring(0, len - 1) |> sprintf "%s…"

            while not token.IsCancellationRequested do
                printf "Command :> "
                let input = Console.ReadLine()
                let args = makeArgs input
                let cmd = CommandLine.parseCommandLine args
                
                match cmd with
                | Import fi ->
                    try
                        let! json = File.ReadAllTextAsync(fi.FullName, Text.Encoding.UTF8) |> Async.AwaitTask
                        let agenda = readAgenda json |> function Ok x -> x | Error e -> raise (new FormatException(e.ToString()))
                        let! response = actor.Process(agenda)
                        printfn "Processed %i alarms" response
                    with
                    | e -> eprintfn "[ERROR] %A" e; return ()
                    
                | List opt ->
                    let sortDomain (xs: seq<Guid * AlarmKey>) = xs |> Seq.sortBy (fun (x,y) -> y.domain) 
                    let sortName (xs: seq<Guid * AlarmKey>) = xs |> Seq.sortBy (fun (x,y) -> y) 
                    let! response =
                        match opt with
                        | ByDomain filter -> actor.ListDomain(filter)
                        | ByName filter   -> actor.ListName(filter)
                    response |> Seq.map (fun (id, ak) -> (id.ToString("N").Substring(19),
                                                          { domain = limit 12 ak.domain; name = limit 30 ak.name }))
                             |> Seq.iter (fun (id, ak) -> printfn "%-12s | %-12s | %-30s" id ak.domain ak.name)
                    response |> Seq.length |> printfn "%d items"
                    
                | Remove opt ->
                    let! response =
                        match opt with
                        | ByDomain filter -> actor.RemoveDomain(filter)
                        | ByName filter   -> actor.RemoveName(filter)
                    printfn "Removed %i alarms" response
                    do! Async.Sleep(300)
                    
                | Help -> ()
        }

        console cts |> Async.Start

        waitForShutdown()
        0

(*
    let main argv =

        let logary =
          Config.create "svc" "localhost"
          |> Config.target (LiterateConsole.create LiterateConsole.empty "nice console")
          |> Config.ilogger (ILogger.LiterateConsole Info)
          |> Config.build
          |> run
        let logger = logary.getLogger "spawnd"
        logary.flushPending() |> run

        let logMessage msgFactory steps result =
            sprintf "Step %2d = %d" steps result |> Message.eventX |> msgFactory
            logary.flushPending() |> run

        let logInfo = logMessage logger.info
        let logDebug = logMessage logger.debug

        use cts = new CancellationTokenSource()
        printfn "Press Ctrl-C to exit..."

        let per str =
            let toDigit ch = int64 ch - int64 '0'
            let rec loop str cnt =
                let total = str |> Seq.map toDigit
                                |> Seq.reduce (*)
                logInfo cnt total
                if total > 9L then loop (string total) (cnt+1)
                else total, cnt
            if String.length str = 0 then 0L, 0
            else loop str 1

        let run () = async {
            let cancel _ = printfn "Quitting inner loop..."
            do AppDomain.CurrentDomain.ProcessExit.Add cancel

            while not cts.IsCancellationRequested do
                printf "\nEnter number: "
                Console.ReadLine() |> per |> ignore
                do! Async.Sleep 100
        }

        run () |> Async.Start

        waitForShutdown ()

        printfn "\nDone."
        0
*)
