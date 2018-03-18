namespace Amber.Spawn

module Configuration = 

    open System
    open System.IO
    open System.String.WrappedString
    open Chiron
    open Utilities

    /// A string of length 100
    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value = let (String100 s) = this in s 

    /// A constructor for strings of length 100
    let private string100 = create singleLineTrimmed (lengthValidator 100) String100 

    /// Converts a wrapped string to a string of length 100
    let private convertTo100 s = apply string100 s

    type Protocol = Http | Https | Ftp | Sftp 

    let private protocol (s: string) = 
        match s.ToLowerInvariant() with 
        | "http"    -> Http 
        | "https"   -> Https 
        | "ftp"     -> Ftp 
        | "sftp"    -> Sftp 
        | _         -> failwith (sprintf "Unknown protocol: %s" s) 

    type Format = Json | Xml | Csv 

    let private format (s: string) = 
        match s.ToLowerInvariant() with 
        | "json"    -> Json 
        | "xml"     -> Xml 
        | "csv"     -> Csv 
        | _         -> failwith (sprintf "Unknown format: %s" s) 

    type Feed = Atom | Rss 

    let private feed (s: string) = 
        match s.ToLowerInvariant() with 
        | "atom"    -> Atom 
        | "rss"     -> Rss 
        | _         -> failwith (sprintf "Unknown feed: %s" s) 

    type RecordType = Assignee | Client | WorkRecord 

    let private recordType (s: string) = 
        match s.ToLowerInvariant() with 
        | "assignee"    -> Assignee 
        | "client"      -> Client 
        | "workrecord"  -> WorkRecord 
        | _             -> failwith (sprintf "Unknown record type: %s" s) 

    let private liftNumber = function 
        | String x -> x 
        | _ -> failwithf "Unknown JSON: %A" json

    type DataSource = {
        name: String100;
        sourceUrl: Uri;
        protocol: Protocol;
        format: Format;
        feed: Feed;
        recordType: RecordType;
        frequencyInSeconds: int;
        batchSize: uint32;
        maxRetries: uint16
    } with static member FromJson (_: DataSource) = json {
            let! n = Json.read "name"
            let! url = Json.read "sourceURL"
            let! p = Json.read "protocol" 
            let! fmt = Json.read "format" 
            let! f = Json.read "feed" 
            let! rt = Json.read "recordType" 
            let! fis = Json.read "frequencyInSeconds" 
            let! b = Json.read "batchSize" 
            let! mr = Json.read "maxRetries" 
            return { name = String100 n; 
                     sourceUrl = Uri url; 
                     protocol = p |> protocol; 
                     format = fmt |> format; 
                     feed = f |> feed; 
                     recordType = rt |> recordType; 
                     frequencyInSeconds = fis |> liftNumber |> int; 
                     batchSize = b |> liftNumber |> uint32; 
                     maxRetries = mr |> liftNumber |> uint16 }
    }

    type Configuration = {
        dataSource: DataSource []
    } with 
        
        static member FromJson(_: Configuration) = json {
            let! c = Json.read "dataSource"
            return { dataSource = c }
        } 
        
        static member ReadConfig(file: FileInfo): Configuration = 
            let read (fi: FileInfo) = 
                use reader = new StreamReader(fi.FullName, true)
                reader.ReadToEnd()

            file |> read |> Json.parse |> Json.deserialize


    type ConfigAgentResponse = 
        | ConfigRead of Guid * Configuration
        | Stopped of DateTimeOffset * string
        | Error of string * Exception option

    type private ConfigAgentMessage = 
        //| WatchFile of FileInfo * ConfigAgentResponse ResultMessage AsyncReplyChannel
        //| WatchFolder of DirectoryInfo * ConfigurationManagerResponse ResultMessage AsyncReplyChannel
        | ReadConfig of FileInfo * ConfigAgentResponse AsyncReplyChannel
        | Stop of ConfigAgentResponse AsyncReplyChannel

    type ConfigAgent() = 

        let agentId = Guid.NewGuid()

        let agent = Agent.Start (fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | ReadConfig (file, ch) -> 
                    let config = file |> Configuration.ReadConfig
                    printfn "%A" config
                    ConfigRead (agentId, config) |> ch.Reply
                    return! loop ()

                | Stop ch ->
                    let response = sprintf "Stopped with %i requests in queue" inbox.CurrentQueueLength
                    Stopped (DateTimeOffset.Now, response) |> ch.Reply
                    return! async.Zero()

            }
            loop ())

        member __.Id with get() = agentId
        
        member __.ReadConfig(fi: FileInfo) =
            let buildMessage chan = ReadConfig (fi, chan)
            agent.PostAndReply buildMessage

        member __.Stop() =
            agent.PostAndReply (Stop)
