namespace Amber.Spawnd.Tests

module Configuration =

    open Amber.Spawn
    open Amber.Spawn.Configuration
    open Xunit
    open FsUnit.Xunit
    open System

    type ``Given valid JSON Configuration`` () = 
        let validJson = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            },{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"xml\",
                \"frequencyInSeconds\": \"4\",
                \"maxRetries\": \"1\",
                \"name\": \"Tiger Atom Feed - #2\",
                \"protocol\": \"http\",
                \"recordType\": \"client\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18152\"
            },{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"csv\",
                \"frequencyInSeconds\": \"4\",
                \"maxRetries\": \"1\",
                \"name\": \"Tiger Atom Feed - #3\",
                \"protocol\": \"https\",
                \"recordType\": \"assignee\",
                \"sourceURL\": \"https://gate.pform.pwc-apps.com:18153\"
            }]
        }"

        let configResult = { 
            dataSource = 
            [|{ 
                batchSize = 100u
                feed = Feed.Atom
                format = Format.Json
                frequencyInSeconds = 3
                maxRetries = 0us
                name = String100 "Tiger Atom Feed - #1"
                protocol = Protocol.Http
                recordType = RecordType.WorkRecord
                sourceUrl = Uri "http://gate.pform.pwc-apps.com:18151"
            };{
                batchSize = 100u
                feed = Feed.Atom
                format = Format.Xml
                frequencyInSeconds = 4
                maxRetries = 1us
                name = String100 "Tiger Atom Feed - #2"
                protocol = Protocol.Http
                recordType = RecordType.Client
                sourceUrl = Uri "http://gate.pform.pwc-apps.com:18152"
            };{
                batchSize = 100u
                feed = Feed.Atom
                format = Format.Csv
                frequencyInSeconds = 4
                maxRetries = 1us
                name = String100 "Tiger Atom Feed - #3"
                protocol = Protocol.Https
                recordType = RecordType.Assignee
                sourceUrl = Uri "https://gate.pform.pwc-apps.com:18153"
            }|]
        }

        [<Fact>]
        member __.``When the json is read, a Configuration record is returned`` () = 
            validJson |> Configuration.convertJsonToConfig |> should equal configResult 
        
        [<Fact>]
        member __.``When converting an equivalent Configuration record, the json should match`` () = 
            configResult |> Configuration.convertConfigToJson 
                         |> Configuration.convertJsonToConfig
                         |> should equal configResult 


    type ``Given invalid JSON Configuration`` () = 
        let invalidName = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"This name contains more characters than the allowed 100 character maximum for the name field in a DataSource record.\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains name longer than 100 chars, an exception is thrown`` () = 
            (fun () -> invalidName |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<Exception>

        let invalidUrl = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"this.is-a.bad-uri.com;18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid url, an URI Format exception is thrown`` () = 
            (fun () -> invalidUrl |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<UriFormatException>

        let invalidProtocol = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"bad-protocol\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid protocol, an exception is thrown`` () = 
            (fun () -> invalidProtocol |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<Exception>

        let invalidFormat = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"bad-format\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid format, an exception is thrown`` () = 
            (fun () -> invalidFormat |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<Exception>

        let invalidFeed = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"bad-feed-type\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid feed, an exception is thrown`` () = 
            (fun () -> invalidFeed |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<Exception>

        let invalidRecordType = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"bad-record-type\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid record type, an exception is thrown`` () = 
            (fun () -> invalidRecordType |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<Exception>

        let invalidBatchSize = "{
            \"dataSource\":[{
                \"batchSize\": \"-1\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid batch size, an exception is thrown`` () = 
            (fun () -> invalidBatchSize |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<OverflowException>

        let invalidFrequency = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"2147483648\",
                \"maxRetries\": \"0\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid frequency, an exception is thrown`` () = 
            (fun () -> invalidFrequency |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<OverflowException>

        let invalidRetries = "{
            \"dataSource\":[{
                \"batchSize\": \"100\",
                \"feed\": \"atom\",
                \"format\": \"json\",
                \"frequencyInSeconds\": \"3\",
                \"maxRetries\": \"65536\",
                \"name\": \"Tiger Atom Feed - #1\",
                \"protocol\": \"http\",
                \"recordType\": \"workrecord\",
                \"sourceURL\": \"http://gate.pform.pwc-apps.com:18151\"
            }]
        }"

        [<Fact>] 
        let ``When the json contains an invalid max retries, an exception is thrown`` () = 
            (fun () -> invalidRetries |> Configuration.convertJsonToConfig |> ignore) 
                |> should throw typeof<OverflowException>
