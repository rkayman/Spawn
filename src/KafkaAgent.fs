namespace Amber.Spawn

module Kafka = 

    open System
    open System.Threading

    type Agent<'T> = MailboxProcessor<'T>

    type FetchMessage<'a> =
        | FetchAtomFeed of ('a -> unit) * 'a * Uri * CancellationTokenSource AsyncReplyChannel
        | Stop of ('a -> unit) * 'a 

    type FetchRequested = 
        | AtomFeedRequested of Uri * string 

    let loggingEvent = Event<FetchRequested>()

    type KafkaAgent<'a>() = 

        let fetchAtomFeed msg receiver url (cts: CancellationTokenSource) = async {
            eprintfn "%A" url
            if cts.IsCancellationRequested then cts.Dispose() else msg |> receiver 
        }

        let stop msg receiver = msg |> receiver

        let requestFetch = Agent.Start(fun inbox -> 
            let rec loop() = async {
                let! msg = inbox.TryReceive() 
                try
                    let cts = new CancellationTokenSource()
                    match msg with 
                    | Some (FetchAtomFeed (receiver, msg:'a, url, replyChannel)) -> 
                        Async.StartImmediate(fetchAtomFeed msg receiver url cts)
                        replyChannel.Reply cts
                        return! loop()
                    
                    | Some (Stop (receiver, msg:'a)) -> 
                        stop msg receiver

                    | None -> return! loop()
                with
                | ex -> 
                    failwithf "[KafkaAgent] Error: %A" ex 
                    loop() |> ignore
            }
            loop())
