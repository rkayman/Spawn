namespace Spawn

open Spawn.Messages
open System.IO
open System

module CommandLine =

    type Command =
        | Import of FileInfo
        | List of RequestOption
        | Remove of RequestOption
        | Version
        | Help
    
    let private badCommand extraMessage = eprintfn "Bad command %s" extraMessage; Help

    let inline private toUpper (s: string) = s.ToUpper()
    
    let inline private equals (x: string) (y: string) = x.Equals(y, StringComparison.OrdinalIgnoreCase)
    
    let inline private isa (xs: string list) (y: string) = xs |> List.exists (equals y)

    let inline private stripQuotes (s: string) = s.Replace("\"", String.Empty, StringComparison.InvariantCulture)
    
    let inline private parseVersion _ = Command.Version

    let inline private parseHelp _ = Help

    let private parseImport args =
        match args with
        | [ x ] when File.Exists(x) -> FileInfo(x) |> Import
        | _ -> badCommand "or missing file"
        
    let private DomainOption = ["--domain"; "-d"]

    let private NameOption   = ["--name"; "-n"]

    let private parseList args =
        match args with
        | [] -> List (ByDomain None)
        | x :: [] when x |> isa DomainOption -> ByDomain None |> List
        | x :: [] when x |> isa NameOption   -> ByName None   |> List
        | x :: y :: [] when x |> isa DomainOption -> Some y |> ByDomain |> List
        | x :: y :: [] when x |> isa NameOption   -> Some y |> ByName   |> List
        | _ -> badCommand ""

    let private parseRemove args =
        match args with
        | [] -> badCommand ""
        | x :: y :: [] when x |> isa DomainOption -> stripQuotes y |> Some |> ByDomain |> Remove
        | x :: y :: [] when x |> isa NameOption   -> stripQuotes y |> Some |> ByName   |> Remove
        | _ -> badCommand ""

    let private commandMap =
        [ ("IMPORT", parseImport); ("LIST", parseList); ("REMOVE", parseRemove); ("VERSION", parseVersion);
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

    let toRequest token cmd =
        match cmd with
        | Import file ->
            async {
                try
                    let! json = File.ReadAllTextAsync(file.FullName, Text.Encoding.UTF8, token) |> Async.AwaitTask
                    return Messages.Import json |> Choice1Of3
                with ex -> return ex |> sprintf "%A" |> Choice3Of3 
            } |> Async.RunSynchronously
            
        | List opt -> Messages.List opt |> Choice1Of3
        
        | Remove opt -> Messages.Remove opt |> Choice1Of3
        
        | Version -> Messages.Version () |> Choice1Of3
        
        | Help ->
            [ ""
              "spawnctl <command> <options>"
              "See latest documentation at https://spawnctl.github.io"
              ""
              "Commands:"
              "\tIMPORT <path>"
              "\tImport the file located at <path>. Format defined at https://spawnctl.github.io"
              ""
              "\tLIST [--domain <filter>]  (* default *)"
              "\tList the known alarms sorted by domain, optionally, matching <filter>"
              ""
              "\tLIST [--name <filter>]"
              "\tList the known alarms sorted by name, optionally, matching <filter>"
              ""
              "\tREMOVE [--domain <filter>]"
              "\tRemove an alarm that matches the filter on alarm domains"
              ""
              "\tREMOVE [--name <filter>]"
              "\tRemove an alarm that matches the filter on alarm names"
              ""
              "\tVERSION"
              "\tRetrieves daemon version information"
              ""
              "\tHELP"
              "\tPrints this text"
              "" ]
            |> Choice2Of3

