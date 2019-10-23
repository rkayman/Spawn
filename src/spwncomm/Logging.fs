namespace Spawn

open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open NodaTime
open System

[<AutoOpen>]
module Logging =

    let private now () = SystemClock.Instance.GetCurrentInstant()
    
    type Event<'a> =
        { id: Guid
          activity: Guid
          causation: Guid
          correlation: Guid
          timestamp: Instant
          name: string
          body: 'a }
        
        override this.ToString() =
            let contents = this.body |> sprintf "%A" |> (fun s -> if s.Length > 40 then s.Substring(0,40) else s)
            sprintf "\"%s\" received at %A\n      Id: {%A}\nActivity: {%A}\n    Corr: {%A}\n   Cause: {%A}\nContents: %s"
                     this.name this.timestamp this.id this.activity this.correlation this.causation contents

    let newEvent activity name body =
        let guid = Guid.NewGuid()
        { id = guid
          activity = activity
          causation = guid
          correlation = guid
          timestamp = now()
          name = name
          body = body }
        
    let nextEvent prev name body =
        let msg' = newEvent prev.activity name body
        { msg' with causation   = prev.id
                    correlation = prev.correlation }
        
    let createConsoleTarget () =
        Config.create (AppDomain.CurrentDomain.FriendlyName) (Environment.MachineName)
        |> Config.target (LiterateConsole.create LiterateConsole.empty "Spawnd console")
        |> Config.ilogger (ILogger.LiterateConsole Debug)
        |> Config.build
        |> run

    let logEvent (log: string -> unit) evt =
        evt |> sprintf "%A" |> log
        evt

    let getLogger (config: LogManager) (name: string) =
        name.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> PointName
        |> config.getLogger
    
    type Log(mgr, name) =
        let logger = getLogger mgr name
        
        let logEventInternal formatter message = formatter message |> logger.logSimple
        
        member this.Error(message) = message |> logEventInternal Message.eventError
        
        member this.Debug(message) = message |> logEventInternal Message.eventDebug
        
        member this.Fatal(message) = message |> logEventInternal Message.eventFatal
        
        member this.Info(message) = message |> logEventInternal Message.eventInfo
        
        member this.Warn(message) = message |> logEventInternal Message.eventWarn
        