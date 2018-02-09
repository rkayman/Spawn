namespace Amber.Spawn

module Configuration = 

    open System
    open System.IO
    open System.String.WrappedString
    open Chiron

    module Types = 

        /// A string of length 100
        type String100 = String100 of string with
            interface IWrappedString with
                member this.Value = let (String100 s) = this in s 

        /// A constructor for strings of length 100
        let string100 = create singleLineTrimmed (lengthValidator 100) String100 

        /// Converts a wrapped string to a string of length 100
        let convertTo100 s = apply string100 s

        type Configuration = {
            dataSource: DataSource []
        } and DataSource = {
            name: String100;
            sourceUrl: Uri;
            protocol: Protocol;
            format: Format;
            feed: Feed;
            recordType: RecordType;
            frequencyInSeconds: uint32;
            batchSize: uint32;
            maxRetries: uint16;
        } and Protocol = Http | Https | Ftp | Sftp 
          and Format = Json | Xml | Csv 
          and Feed = Atom | Rss 
          and RecordType = Assignee | Client | WorkRecord 


    open Types

    let private protocol (s: string) = 
        match s.ToLowerInvariant() with 
        | "http"    -> Http 
        | "https"   -> Https 
        | "ftp"     -> Ftp 
        | "sftp"    -> Sftp 
        | _         -> failwith (sprintf "Unknown protocol: %s" s) 

    let private format (s: string) = 
        match s.ToLowerInvariant() with 
        | "json"    -> Json 
        | "xml"     -> Xml 
        | "csv"     -> Csv 
        | _         -> failwith (sprintf "Unknown format: %s" s) 
    
    let private feed (s: string) = 
        match s.ToLowerInvariant() with 
        | "atom"    -> Atom 
        | "rss"     -> Rss 
        | _         -> failwith (sprintf "Unknown feed: %s" s) 

    let private recordType (s: string) = 
        match s.ToLowerInvariant() with 
        | "assignee"    -> Assignee 
        | "client"      -> Client 
        | "workrecord"  -> WorkRecord 
        | _             -> failwith (sprintf "Unknown record type: %s" s) 

    type DataSource with 
        static member FromJson (_: DataSource) = json {
            let! n = Json.read "name"
            let! url = Json.read "sourceUrl"
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
                     frequencyInSeconds = fis; 
                     batchSize = b; 
                     maxRetries = mr }
        }

    // type Configuration with 
    //     // TODO: Change to account for array of data sources
    //     static member FromJson (_: Configuration) = json {
    //         let! c = Json.read "dataSource"
    //         return { dataSource = c }
    //     } 

    // let private configFromJson = function 
    //     | Object ds -> Value ds
    //     | json -> 
    //         Json.formatWith JsonFormattingOptions.SingleLine json
    //         |> sprintf "Expected a string containing a configuration: %s"
    //         |> Error

    // let private fromJsonFoldWith deserialize fold zero xs =
    //     List.fold (fun r x ->
    //       match r with
    //       | Error e -> Error e
    //       | Value xs ->
    //         match deserialize x with
    //         | Value x -> Value (fold x xs)
    //         | Error e -> Error e) (Value zero) (List.rev xs)

    // let private listFromJsonWith deserialize = function
    //   | Array lst -> fromJsonFoldWith deserialize (fun x xs -> x::xs) [] lst
    //   | _ -> failwith "Expected an array"

    open FSharp.Data

    type private Config = 
        JsonProvider<"../configs/amberPipe-atom.json", InferTypesFromValues=true, RootName="Config">

    let private readConfig (file: FileInfo) = 
        use reader = new StreamReader(file.FullName, true)
        reader.ReadToEnd()

    // let getConfiguration (file: FileInfo) : Configuration = 
        //file |> readConfig |> Json.parse |> Json.deserialize
    let getConfiguration (file: FileInfo) = 
        use reader = new StreamReader(file.FullName, true) 
        let config = Config.Load(reader)
        printfn "%A" config.JsonValue
        printfn "%A" config.DataSource
