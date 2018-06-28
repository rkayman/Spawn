namespace Amber.Spawn 

module CommandLine = 

    open System
    open System.IO
    open Logary

    let usageMsg = "\nusage: Spawn [options] \
                    \n  -h | --help \t\t\t\tPrint this help message. \
                    \n  --config <file> \t\t\tRetrieve configuration from [configFile] \
                    \n  --kafka-topic <name> \t\t\tName of Kafka topic \
                    \n  --kafka-host <name|addr[:port]> \tHost name or IP address and port\n"

    let private logger = Logging.getCurrentLogger ()

    type Options = {
        config: FileInfo option 
        host: Uri option
        topic: string option
    }

    type OptionArgument = Config | Host | Topic | Missing | Unknown | NoOptions

    type OptionError = {
        argument: OptionArgument
        message: string
    } with
        member this.PrintError = 
            match this.argument with
            | NoOptions -> this.message |> Message.eventFatal |> logger.logSimple
            | _ -> sprintf "%A option: %s" this.argument this.message
                   |> Message.eventFatal |> logger.logSimple

    type OptionErrors = {
        errors: OptionError list
    } with 
        member this.PrintErrors = 
            this.errors |> List.iter (fun x -> x.PrintError)

    type OptionsResult = Help | Options of Options | Errors of OptionErrors

    let rec parseRec options args =
        match args with
        | [] -> 
            match options with
            | { config = None; host = _; topic = _ } 
            | { config = _; host = None; topic = _ } 
            | { config = _; host = _; topic = None } -> 
                let err = { argument = Missing; message = "Missing argument(s)" }
                Errors { errors = [err] }
            | _ -> Options options

        | "-h"::_ | "--help"::_ | "/h"::_ | "/help"::_ | "-?"::_ -> Help

        | "-c"::args' | "/c"::args' | "--config"::args' | "/config"::args' ->
            match args' with
            | x::args'' ->
                if File.Exists(x) then 
                    args'' |> parseRec { options with config = Some (FileInfo x) }
                else 
                    let err = { argument = Config; 
                                message = "Unable to find configuration file." } 
                    Errors { errors = [err] }
            | _ ->
                let err = { argument = Config; 
                            message = "Please specify a configuration file" }
                Errors { errors = [err] }

        | "-kh"::args' | "/kh"::args' | "--kafka-host"::args' | "/kafka-host"::args' ->
            match args' with
            | x::args'' ->
                match Uri.TryCreate(x, UriKind.RelativeOrAbsolute) with
                | true, uri -> args'' |> parseRec { options with host = Some uri }
                | false, _ -> 
                    let err = { argument = Host;
                                message = "Missing Kafka host name or ip address" }
                    Errors { errors = [err] }
            | _ -> 
                let err = { argument = Host; message = "Please specify a Kafka host name or ip address" }
                Errors { errors = [err] }

        | "-kt"::args' | "/kt"::args' | "--kafka-topic"::args' | "/kafka-topic"::args' ->
            match args' with
            | x::args'' ->
                if not (String.IsNullOrWhiteSpace(x)) then
                    args'' |> parseRec { options with topic = Some x }
                else
                    let err = { argument = Topic;
                                message = "Missing Kafka topic"}
                    Errors { errors = [err] }
            | _ -> 
                let err = { argument = Topic; message = "Please specify a Kafka topic name" }
                Errors { errors = [err] }

        | x -> 
            let err = { argument = Unknown; message = sprintf "Invalid command (%s)" x.[0] }
            Errors { errors = [err] }

    let parse argv = 
        let options = { config = None; host = None; topic = None }
        argv |> Array.toList |> parseRec options 
