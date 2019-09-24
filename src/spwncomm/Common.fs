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
    with
        static member JsonObjCodec =
            jchoice [
                ByDomain <!> jreq "by-domain" (function ByDomain x -> Some x | _ -> None)
                ByName   <!> jreq "by-name"   (function ByName x -> Some x | _ -> None)
            ]

    type Json = string
    
    type Request =
        | Import of Json
        | List of RequestOption
        | Remove of RequestOption
        | Version of unit
    with
        static member JsonObjCodec =
            jchoice [
                Import  <!> jreq "import"  (function Import json -> Some json | _ -> None)
                List    <!> jreq "list"    (function List opt -> Some opt | _ -> None)
                Remove  <!> jreq "remove"  (function Remove opt -> Some opt | _ -> None)
                Version <!> jreq "version" (function Version _ -> Some () | _ -> None)
            ]

        static member Deserialize(json) : Result<Request,_> = parseJson json |> Result.mapError (sprintf "%A")
        
        member this.Serialize() = this |> toJson |> string |> Ok

    type ImportedInfo =
        { total: int
          distinct: int }
    with
        static member JsonObjCodec =
            fun t d -> { total = t; distinct = d }
            <!> jreq    "total"    (fun x -> Some x.total)
            <*> jreq    "distinct" (fun x -> Some x.distinct)
            
    let private instantToJson (x: Instant) = x |> NodaTime.Text.InstantPattern.General.Format |> JString
    
    let private (|Instant|_|) (str: string) =
        match NodaTime.Text.InstantPattern.General.Parse(str).TryGetValue(Instant.MinValue) with
        | true, instant -> Some instant
        | false, _ -> None

    let private jsonToInstant = function
        | JString x as json -> match x with
                               | Instant z -> Decode.Success z
                               | _ -> Decode.Fail.invalidValue json x
        | json -> Decode.Fail.strExpected json
        
    let private instantCodec = jsonToInstant, instantToJson
    
    type AlarmInfo =
        { id: Guid
          domain: string
          name: string
          count: int64
          last: Instant option }
    with
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
    with
        static member JsonObjCodec =
            jchoice [
                Imported        <!> jreq "imported" (function Imported x -> Some x | _ -> None)
                Listed          <!> jreq "listed"   (function Listed x -> Some x | _ -> None)
                Removed         <!> jreq "removed"  (function Removed x -> Some x | _ -> None)
                VersionReported <!> jreq "version"  (function VersionReported x -> Some x | _ -> None)
            ]
            
        static member Deserialize(json) : Result<Response,_> = parseJson json |> Result.mapError (sprintf "%A")
            
        member this.Serialize() = this |> toJson |> string |> Ok


module Common =
            
    let internal lift = function Ok x -> x | Error e -> failwithf "%A" e
    
    type System.Guid with
        member this.ToShortString() = this.ToString("N").Substring(20)

    type Agent<'T> = MailboxProcessor<'T>
    
    type AutoCancelActor<'T>(body: (Agent<'T> -> Async<unit>), ?token: CancellationToken) =
        let cts = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource(t))
                        |> Option.defaultValue (new CancellationTokenSource())
        let mutable disposed = false
        
        let actor = Agent<'T>.Start(body, cts.Token)
        
        member this.Agent with get() = actor
        
        member this.Token with get() = cts.Token
        
        abstract member Dispose : unit -> unit
        default this.Dispose() = (this :> IDisposable).Dispose()

        interface IDisposable with
            member this.Dispose() =
                if not disposed then
                    disposed <- true
//                    cts.Cancel()
                    (actor :> IDisposable).Dispose()
                    cts.Dispose()
