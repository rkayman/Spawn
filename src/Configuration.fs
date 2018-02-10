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
            frequencyInSeconds: uint32;
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
                         frequencyInSeconds = fis |> liftNumber |> uint32; 
                         batchSize = b |> liftNumber |> uint32; 
                         maxRetries = mr |> liftNumber |> uint16 }
        }

        type Configuration = {
            dataSource: DataSource []
        } with static member FromJson (_: Configuration) = json {
                let! c = Json.read "dataSource"
                return { dataSource = c }
        } 

    open Types

    let private readConfig (file: FileInfo) = 
        use reader = new StreamReader(file.FullName, true)
        reader.ReadToEnd()

    let getConfiguration (file: FileInfo) : Configuration = 
        file |> readConfig |> Json.parse |> Json.deserialize
