﻿namespace Amber.Spawn

open System

module App =

    open CommandLine

    [<EntryPoint>]
    let main argv =
        let args = Array.toList argv
        let result = parse args
        let printHelp() = helpMsg |> eprintfn "%s" 

        match result with 
        | Help -> printHelp()
        
        | Errors errors -> 
            errors.PrintErrors 
            printHelp()

        | Options options -> 
            eprintfn "\nUsing configuration found in %A...\n" options.file.Value.FullName 
            // TODO: read configuration from file
            // TODO: create agents (actors) using configuration file
            // TODO: consider creating a parent agent to send 'kill' message to all child agents
            printf "> "
            let rec loop () = 
                let input = Console.ReadKey(true)
                match input.Key with 
                | ConsoleKey.Q | ConsoleKey.Escape -> 
                    // TODO: cancel all actors (wait for them to finish)
                    printf "\n...quitting..."
                | _ -> 
                    loop ()
            loop ()
            printfn "done\n"

        0 // return an integer exit code