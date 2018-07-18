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
    let private convertTo100 s = 
        match string100 s with 
        | Some s' -> s' 
        | _ -> failwith (sprintf "[CONFIGURATION ERROR] Name is too long: %s" s)

    let private string100ToJson (s: String100) = String <| value s 

    let private uriToJson (u: Uri) = String <| u.ToString() 

    type Protocol = Http | Https | Ftp | Sftp 

    let private protocol (s: string) = 
        match s.ToLowerInvariant() with 
        | "http"    -> Http 
        | "https"   -> Https 
        | "ftp"     -> Ftp 
        | "sftp"    -> Sftp 
        | _         -> failwith (sprintf "[CONFIGURATION ERROR] Unknown protocol: %s" s) 

    let private protocolToJson (p: Protocol) = String <| p.ToString().ToLowerInvariant() 

    type Format = Json | Xml | Csv 

    let private format (s: string) = 
        match s.ToLowerInvariant() with 
        | "json"    -> Json 
        | "xml"     -> Xml 
        | "csv"     -> Csv 
        | _         -> failwith (sprintf "[CONFIGURATION ERROR] Unknown format: %s" s) 

    let private formatToJson (f: Format) = String <| f.ToString().ToLowerInvariant() 

    type Feed = Atom | Rss 

    let private feed (s: string) = 
        match s.ToLowerInvariant() with 
        | "atom"    -> Atom 
        | "rss"     -> Rss 
        | _         -> failwith (sprintf "[CONFIGURATION ERROR] Unknown feed: %s" s) 

    let private feedToJson (f: Feed) = String <| f.ToString().ToLowerInvariant() 

    type RecordType = Assignee | Client | WorkRecord 

    let private recordType (s: string) = 
        match s.ToLowerInvariant() with 
        | "assignee"    -> Assignee 
        | "client"      -> Client 
        | "workrecord"  -> WorkRecord 
        | _             -> failwith (sprintf "[CONFIGURATION ERROR] Unknown record type: %s" s) 

    let private recordTypeToJson (x: RecordType) = String <| x.ToString().ToLowerInvariant()  
     
    let private liftNumber json = 
        match json with 
        | String x -> x 
        | Number x -> string x
        | _ -> failwith (sprintf "[CONFIGURATION ERROR] Invalid number: %A" json) 

    type DataSource = {
        batchSize: uint32
        feed: Feed
        format: Format
        frequencyInSeconds: int
        maxRetries: uint16 
        name: String100
        protocol: Protocol
        recordType: RecordType
        sourceUrl: Uri
    } with 

        static member FromJson (_: DataSource) = json {
            let! b = Json.read "batchSize" 
            let! f = Json.read "feed" 
            let! fmt = Json.read "format" 
            let! fis = Json.read "frequencyInSeconds" 
            let! mr = Json.read "maxRetries" 
            let! n = Json.read "name"
            let! p = Json.read "protocol" 
            let! rt = Json.read "recordType" 
            let! url = Json.read "sourceURL"
            return { 
                batchSize = b |> liftNumber |> uint32 
                feed = f |> feed 
                format = fmt |> format 
                frequencyInSeconds = fis |> liftNumber |> int 
                maxRetries = mr |> liftNumber |> uint16 
                name = n |> convertTo100 
                protocol = p |> protocol 
                recordType = rt |> recordType 
                sourceUrl = Uri url 
            }
        } 

        static member ToJson (x: DataSource) = json {
            do! Json.write "batchSize" x.batchSize 
            do! Json.writeWith feedToJson "feed" x.feed 
            do! Json.writeWith formatToJson "format" x.format 
            do! Json.write "frequencyInSeconds" x.frequencyInSeconds 
            do! Json.write "maxRetries" x.maxRetries 
            do! Json.writeWith string100ToJson "name" x.name
            do! Json.writeWith protocolToJson "protocol" x.protocol 
            do! Json.writeWith recordTypeToJson "recordType" x.recordType 
            do! Json.writeWith uriToJson "sourceURL" x.sourceUrl 
        }

    type Configuration = {
        dataSource: DataSource []
    } with 

        static member FromJson (_: Configuration) = json {
            let! c = Json.read "dataSource"
            return { dataSource = c }
        } 

        static member ToJson (x: Configuration) = json {
            do! Json.write "dataSource" x.dataSource
        }

    let convertJsonToConfig json : Configuration = json |> Json.parse |> Json.deserialize 

    let convertConfigToJson (cfg: Configuration) = cfg |> Json.serialize 
                                                       |> Json.format 

    let private readFile (file: FileInfo) = 
        use reader = new StreamReader(file.FullName, true) 
        reader.ReadToEnd() 

    let readConfigFile = readFile >> convertJsonToConfig


    type ConfigAgentResponse = 
        | ConfigRead of Guid * Configuration
        | Stopped of DateTimeOffset * string
        | Error of string * Exception option

    type private ConfigAgentMessage = 
        //| WatchFile of FileInfo * ConfigAgentResponse ResultMessage AsyncReplyChannel
        //| WatchFolder of DirectoryInfo * ConfigurationManagerResponse ResultMessage AsyncReplyChannel
        | ReadConfig of string * ConfigAgentResponse AsyncReplyChannel
        | ReadConfigFile of FileInfo * ConfigAgentResponse AsyncReplyChannel
        | Stop of ConfigAgentResponse AsyncReplyChannel

    type ConfigAgent() = 

        let agentId = Guid.NewGuid()

        let agent = Agent.Start (fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | ReadConfig (json, ch) -> 
                    let config = json |> convertJsonToConfig
                    ConfigRead (agentId, config) |> ch.Reply 
                    return! loop ()

                | ReadConfigFile (file, ch) -> 
                    let config = file |> readConfigFile 
                    ConfigRead (agentId, config) |> ch.Reply
                    return! loop ()

                | Stop ch ->
                    let response = sprintf "Stopped with %i requests in queue" inbox.CurrentQueueLength
                    Stopped (DateTimeOffset.Now, response) |> ch.Reply
                    return! async.Zero()

            }
            loop ())

        member __.Id with get() = agentId

        member __.ReadConfig(json: string) = 
            let buildMessage ch = ReadConfig (json, ch) 
            agent.PostAndAsyncReply buildMessage 
        
        member __.ReadConfig(fi: FileInfo) =
            let buildMessage ch = ReadConfigFile (fi, ch)
            agent.PostAndAsyncReply buildMessage 

        member __.Stop() =
            agent.PostAndAsyncReply (Stop)
