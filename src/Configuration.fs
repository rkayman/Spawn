namespace Amber.Spawn

module Configuration = 

    open System
    open System.IO

    type private Configuration = {
        dataSource: DataSource
    } and DataSource = {
        name: string;
        sourceUrl: Uri;
        protocol: Protocol;
        format: Format;
        feed: Feed;
        recordType: RecordType;
        frequencyInSeconds: uint32;
        batchSize: uint32;
        maxRetries: uint8;
    } and Name = 
        | Name of string 
      and Protocol = 
        | HTTP | HTTPS | FTP | FTPS  
      and Format = 
        | JSON | XML | CSV 
      and Feed = 
        | ATOM | RSS 
      and RecordType = 
        | Assignee | Client | WorkRecord 

    let getConfiguration (file: FileInfo) = ()
