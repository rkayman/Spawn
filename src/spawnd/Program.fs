namespace Spawn

open System
open System.IO
open System.Threading

module Program =

    module CommandLine =

        type ListOptions = | ByDomain | ByName
        type RemoveOptions =
            | Domain of string
            | Name of string
            | Id of int

        type CommandLineCommand =
            | Import of FileInfo
            | List of ListOptions
            | Remove of RemoveOptions
            | Dump
            | Help

        let private badCommand extraMessage = eprintfn "Bad command %s" extraMessage; Help

        let private toLower (s: string) = s.ToLowerInvariant()

        let inline private stripQuotes (s: string) = s.Replace("\"", String.Empty, StringComparison.InvariantCulture)

        let private parseDump _ = Dump

        let private parseHelp _ = Help

        let private parseImport args =
            match args with
            | [ x ] when File.Exists(x) -> FileInfo(x) |> Import
            | _ -> badCommand "or missing file"

        let private parseList args =
            match args with
            | [] -> List ByDomain
            | x :: [] when toLower x = "bydomain" -> List ByDomain
            | x :: [] when toLower x = "byname" -> List ByName
            | _ -> badCommand ""

        let private parseRemove args =
            match args with
            | [] -> badCommand ""
            | x :: y :: [] when toLower x = "domain" -> Remove(Domain(stripQuotes y))
            | x :: y :: [] when toLower x = "name" -> Remove(Name(stripQuotes y))
            | x :: y :: [] when toLower x = "id" -> Remove(Id(int y))
            | _ -> badCommand ""

        let private commandMap =
            [ ("import", parseImport); ("list", parseList); ("remove", parseRemove); ("dump", parseDump);
              ("help", parseHelp); ("/h", parseHelp); ("-h", parseHelp); ("-?", parseHelp); ("/?", parseHelp) ]
            |> Map.ofSeq

        let private isCommand str = str |> toLower |> commandMap.ContainsKey

        let parseCommandLine args =
            match args with
            | [] -> badCommand ""
            | x :: xs when isCommand x ->
                let invApply x f = f x
                let parser = invApply xs
                commandMap |> Map.find x |> parser
            | _ -> badCommand ""

    [<EntryPoint>]
    let main _ =

        use cts = new CancellationTokenSource()
        
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
                              |> fun (x, y, z) -> z

        let console() = async {
            let ws = [| ' '; '\t'; '\n'; '\r' |]
            let cancel _ = printfn "Quitting console..."
            do AppDomain.CurrentDomain.ProcessExit.Add cancel

            while not cts.IsCancellationRequested do
                printf "Command > "
                let input = Console.ReadLine()
                let args = makeArgs input
                printfn "%A" args
                let cmd = CommandLine.parseCommandLine args
                printfn "%A" cmd
        }

        console () |> Async.Start

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
