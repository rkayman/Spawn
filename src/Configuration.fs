namespace Amber.Spawn

module Configuration = 

    open System
    open System.IO
    open System.String.WrappedString

    module TypeExtensions = 

        /// A string of length 100
        type String100 = String100 of string with
            interface IWrappedString with
                member this.Value = let (String100 s) = this in s

        /// A constructor for strings of length 100
        let string100 = create singleLineTrimmed (lengthValidator 100) String100 

        /// Converts a wrapped string to a string of length 100
        let convertTo100 s = apply string100 s

        /// A string of length 50
        type String50 = String50 of string with
            interface IWrappedString with
                member this.Value = let (String50 s) = this in s

        /// A constructor for strings of length 50
        let string50 = create singleLineTrimmed (lengthValidator 50)  String50

        /// Converts a wrapped string to a string of length 50
        let convertTo50 s = apply string50 s

    open TypeExtensions

    type private Configuration = {
        dataSource: DataSource
    } and DataSource = {
        name: String100;
        sourceUrl: Uri;
        protocol: Protocol;
        format: Format;
        feed: Feed;
        recordType: RecordType;
        frequencyInSeconds: uint32;
        batchSize: uint32;
        maxRetries: uint8;
    } and Protocol = 
        | HTTP | HTTPS | FTP | FTPS  
      and Format = 
        | JSON | XML | CSV 
      and Feed = 
        | ATOM | RSS 
      and RecordType = 
        | Assignee | Client | WorkRecord 

    let getConfiguration (file: FileInfo) = ()
