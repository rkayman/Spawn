namespace Spawn

open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open System

[<AutoOpen>]
module Logging =

    let private logary =
      Config.create (AppDomain.CurrentDomain.FriendlyName) (Environment.MachineName)
      |> Config.target (LiterateConsole.create LiterateConsole.empty "Spawnd console")
      |> Config.ilogger (ILogger.LiterateConsole Debug)
      |> Config.build
      |> run

    let getLogger (name: string) = name.Split('.', StringSplitOptions.RemoveEmptyEntries)
                                   |> PointName
                                   |> logary.getLogger
    
    type Log(name) =
        let logger = getLogger name
        
        let logEvent formatter message = formatter message |> logger.logSimple
        
        member this.Error(message) = message |> logEvent Message.eventError
        
        member this.Debug(message) = message |> logEvent Message.eventDebug
        
        member this.Fatal(message) = message |> logEvent Message.eventFatal
        
        member this.Info(message) = message |> logEvent Message.eventInfo
        
        member this.Warn(message) = message |> logEvent Message.eventWarn
        