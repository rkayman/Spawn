namespace Amber.Spawn 

module CommandLine = 
    let helpMsg = "\nusage: Spawn [options] \
                   \n  -h | --help \t\tPrint this help message. \
                   \n  --config [file] \tRetrieve configuration from [configFile]\n"

    open System.IO

    type Options = {
        file: FileInfo option 
    }

    type OptionArgument = File | Missing | Unknown | NoOptions

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
           let err = { argument = Missing; message = "Missing argument(s)" }
           Errors { errors = [err] }

        | "-h"::_ | "--help"::_ | "/h"::_ | "/help"::_ | "-?"::_ -> Help

        | "-c"::args' | "/c"::args' | "--config"::args' ->
            match args' with
            | x::_ ->
                if File.Exists(x) then 
                    Options { options with file = Some (FileInfo x) }
                else 
                    let err = { argument = File; 
                                message = "Unable to find configuration file." } 
                    Errors { errors = [err] }
            | _ ->
                let err = { argument = File; 
                            message = "Please specify a configuration file" }
                Errors { errors = [err] }
        | x -> 
            let err = { argument = Unknown; message = sprintf "Invalid command (%s)" x.[0] }
            Errors { errors = [err] }

    let parse argv = 
        let options = { file = None }
        parseRec argv options 
