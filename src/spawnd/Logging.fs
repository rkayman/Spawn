namespace Spawn

open Hopac
open Logary
open Logary.Message
open Logary.Configuration
open Logary.Targets
open System

[<AutoOpen>]
module Logging =

    let logary =
      Config.create (AppDomain.CurrentDomain.FriendlyName) (Environment.MachineName)
      |> Config.target (LiterateConsole.create LiterateConsole.empty "Spawnd console")
      |> Config.ilogger (ILogger.LiterateConsole Debug)
      |> Config.build
      |> run

