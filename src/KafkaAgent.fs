namespace Amber.Spawn

module Kafka = 

    open System
    open System.Threading
    open Configuration

    type Agent<'T> = MailboxProcessor<'T>

    type FetchMessage =
        | Init of CancellationTokenSource AsyncReplyChannel
        | FetchAtomFeed of Uri * RecordType * String100 
        | Stop 

    type KafkaAgent() = 

        let mutable (cts: CancellationTokenSource option) = None 

        let requestAtomFeedFetch url recordType (name: String100) = async {
            if Option.isSome cts && cts.Value.IsCancellationRequested then 
                cts.Value.Dispose() 
            else 
                eprintfn "Fetching %A from '%A' using %A" recordType name url 
        }

        let stop() = eprintfn "Stopping KafkaAgent..."

        let requestFetch = Agent.Start(fun inbox -> 
            let rec loop() = async {
                try
                    let! msg = inbox.Receive() 
                    match msg with 
                    | Some (Init (replyChannel)) -> 
                        if Option.isNone cts then 
                            cts <- Some (new CancellationTokenSource())
                            replyChannel.Reply cts.Value
                            return! loop()
                        else 
                            invalidOp "KafkaAgent already initialized. Cannot initialize twice."
                        
                    | Some (FetchAtomFeed (url, recordType, name)) -> 
                        Async.StartImmediate(requestAtomFeedFetch url recordType name)
                        return! loop()
                    
                    | Some Stop -> 
                        stop()
                        cts.Value.Dispose()

                    | None -> return! loop()
                with
                | ex -> 
                    failwithf "[KafkaAgent] Error: %A" ex 
                    return! loop()
            }
            loop())
