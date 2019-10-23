namespace Spawn

open FSharpPlus
open Fleece.FSharpData.Operators
open Fleece.FSharpData
open NodaTime
open System.Threading
open System


module Messages =
    
    type RequestOption =
        | ByDomain of string option
        | ByName of string option
        | ById of string option

        static member JsonObjCodec =
            jchoice [
                ByDomain <!> jreq "by-domain" (function ByDomain x -> Some x | _ -> None)
                ByName   <!> jreq "by-name"   (function ByName x -> Some x | _ -> None)
                ById     <!> jreq "by-id"     (function ById x -> Some x | _ -> None)
            ]

    type Json = string
    
    type Request =
        | Import of Json
        | List of RequestOption
        | Remove of RequestOption
        | Version of unit
        
        override this.ToString() =
            match this with
            | Import x   -> sprintf "Import %d bytes of json" x.Length
            | List x     -> sprintf "List %A" x
            | Remove x   -> sprintf "Remove %A" x
            | Version () -> sprintf "Show version" 

        static member JsonObjCodec =
            jchoice [
                Import  <!> jreq "import"  (function Import json -> Some json | _ -> None)
                List    <!> jreq "list"    (function List opt -> Some opt | _ -> None)
                Remove  <!> jreq "remove"  (function Remove opt -> Some opt | _ -> None)
                Version <!> jreq "version" (function Version _ -> Some () | _ -> None)
            ]

        static member Deserialize(json) : Request =
            parseJson json
            |> Result.either (id) (sprintf "%A" >> invalidOp)

        member this.Serialize() = this |> toJson |> string
        
        member this.ActivityId with get() = match this with
                                            | Import _  -> new Guid("C82D9F51-662C-4B88-A351-7A1B9BD6EE4B")
                                            | List _    -> new Guid("FA2E7A80-9701-4473-BAEE-347D9847147B")
                                            | Remove _  -> new Guid("788FE8D2-C3BD-4307-8853-593DA10814EF")
                                            | Version _ -> new Guid("118E30CD-124D-4061-BAF5-1D183A52E266")
            
        member this.Name with get() = match this with
                                      | Import _  -> "ImportRequested"
                                      | List _    -> "ListRequested"
                                      | Remove _  -> "RemoveRequested"
                                      | Version _ -> "VersionRequested"
            
    type ImportedInfo =
        { total: int
          distinct: int }

        static member JsonObjCodec =
            fun t d -> { total = t; distinct = d }
            <!> jreq    "total"    (fun x -> Some x.total)
            <*> jreq    "distinct" (fun x -> Some x.distinct)
            
    let internal instantToJson (x: Instant) = x |> NodaTime.Text.InstantPattern.General.Format |> JString
    
    let internal (|Instant|_|) (str: string) =
        match NodaTime.Text.InstantPattern.General.Parse(str).TryGetValue(Instant.MinValue) with
        | true, instant -> Some instant
        | false, _ -> None

    let internal jsonToInstant = function
        | JString x as json -> match x with
                               | Instant z -> Decode.Success z
                               | _ -> Decode.Fail.invalidValue json x
        | json -> Decode.Fail.strExpected json
        
    let internal instantCodec = jsonToInstant, instantToJson
    
    type AlarmInfo =
        { id: Guid
          domain: string
          name: string
          count: int64
          last: Instant option }

        static member JsonObjCodec =
            fun i d n c l -> { id = i; domain = d; name = n; count = c; last = l }
            <!> jreq                     "id"      (fun x -> Some x.id)
            <*> jreq                     "domain"  (fun x -> Some x.domain)
            <*> jreq                     "name"    (fun x -> Some x.name)
            <*> jreq                     "count"   (fun x -> Some x.count)
            <*> joptWith instantCodec    "last"    (fun x -> x.last)
        
    type Response =
        | Imported of ImportedInfo
        | Listed of AlarmInfo[]
        | Removed of int
        | VersionReported of string
        
        override this.ToString() =
            match this with
            | Imported x        -> sprintf "Imported %d of %d alarms" x.distinct x.total
            | Listed x          -> sprintf "Listed %d alarms" x.Length
            | Removed x         -> sprintf "Removed %d alarms" x
            | VersionReported x -> sprintf "Reported version = %s" x

        static member JsonObjCodec =
            jchoice [
                Imported        <!> jreq "imported" (function Imported x -> Some x | _ -> None)
                Listed          <!> jreq "listed"   (function Listed x -> Some x | _ -> None)
                Removed         <!> jreq "removed"  (function Removed x -> Some x | _ -> None)
                VersionReported <!> jreq "version"  (function VersionReported x -> Some x | _ -> None)
            ]
            
        static member Deserialize(json) : Response =
            parseJson json
            |> Result.either (id) (sprintf "%A" >> invalidOp)
    
        member this.Serialize() = this |> toJson |> string
            

module Common =    
    type System.Guid with
        member this.ToShortString() = this.ToString("N").Substring(20)

    type Agent<'T> = MailboxProcessor<'T>
    
    type AutoCancelActor<'T>(body: (Agent<'T> -> Async<unit>), log: Logging.Log, ?token: CancellationToken) =
        let cts = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource([|t|]))
                        |> Option.defaultValue (new CancellationTokenSource())
        let mutable disposed = false
        
        let actor = Agent<'T>.Start(body, cts.Token)
        
        member this.Agent with get() = actor
        
        member this.Token with get() = cts.Token
        
        abstract member Dispose : unit -> unit
        default this.Dispose() = (this :> IDisposable).Dispose()

        interface IDisposable with
            member this.Dispose() =
                "Disposing actor" |> log.Debug
                if not disposed then
                    try
                        disposed <- true
                        if not cts.IsCancellationRequested then cts.Cancel()
                        (actor :> IDisposable).Dispose()
                        cts.Dispose()
                        "Actor disposed" |> log.Debug
                    with
                        | :? ObjectDisposedException -> ()
                        