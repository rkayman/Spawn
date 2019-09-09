namespace Spawn.IO

open System.IO

module Messages =
    
    type CommandOption = ByDomain of string option | ByName of string option

    type Command =
        | Import of FileInfo
        | List of CommandOption
        | Remove of CommandOption
        | Help

    