namespace Amber.Spawn 

module CommandLine = 

    open System.IO

    let usageMsg = "\nusage: Spawn [options] \
                    \n  -h | --help \t\tPrint this help message. \
                    \n  --config [file] \tRetrieve configuration from [configFile] \
                    \n  --log [file] \t\tSend logging data to file\n"

    type Options = {
        file: FileInfo option 
        log: FileInfo option
    }

    type OptionArgument = File | Log | Missing | Unknown | NoOptions

    type OptionError = {
        argument: OptionArgument
        message: string
    } with
        member this.PrintError = 
            match this.argument with
            | NoOptions -> eprintfn "%s" this.message
            | _ -> eprintfn "%A option: %s" this.argument this.message

    type OptionErrors = {
        errors: OptionError list
    } with 
        member this.PrintErrors = 
            this.errors |> List.iter (fun x -> x.PrintError)

    type OptionsResult = Help | Options of Options | Errors of OptionErrors

    let rec parseRec args options =
        match args with
        | [] -> 
            match options with
            | { file = None; log = _ } -> 
                let err = { argument = Missing; message = "Missing argument(s)" }
                Errors { errors = [err] }
            | _ -> Options options

        | "-h"::_ | "--help"::_ | "/h"::_ | "/help"::_ | "-?"::_ -> Help

        | "-c"::args' | "/c"::args' | "--config"::args' | "/config"::args' ->
            match args' with
            | x::args'' ->
                if File.Exists(x) then 
                    parseRec args'' { options with file = Some (FileInfo x) }
                else 
                    let err = { argument = File; 
                                message = "Unable to find configuration file." } 
                    Errors { errors = [err] }
            | _ ->
                let err = { argument = File; 
                            message = "Please specify a configuration file" }
                Errors { errors = [err] }

        | "-l"::args' | "/l"::args' | "--log"::args' | "/log"::args' ->
            match args' with
            | x::args'' ->
                try
                    let log = FileInfo x
                    parseRec args'' { options with log = Some log }
                with
                | ex -> 
                    let err = { argument = Log; message = ex.Message }
                    Errors { errors = [err] }
            | _ -> 
                let err = { argument = Log; message = "Please specify a log file"}
                Errors { errors = [err] }

        | x -> 
            let err = { argument = Unknown; message = sprintf "Invalid command (%s)" x.[0] }
            Errors { errors = [err] }

    let parse argv = 
        let options = { file = None; log = None }
        parseRec argv options 
