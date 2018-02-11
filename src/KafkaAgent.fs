namespace Amber.Spawn

module Kafka = 

    open System
    open System.Threading

    type Agent<'T> = MailboxProcessor<'T>

    type FetchMessage<'a> =
        | FetchAtomFeed of ('a -> unit) * 'a * Uri * CancellationTokenSource
        | Stop of ('a -> unit) * 'a 

    type FetchRequested = 
        | AtomFeedRequested of Uri * string 

    let loggingEvent = Event<FetchRequested>()

    type KafkaAgent<'a>() = 

        let fetchAtomFeed msg receiver url (cts: CancellationTokenSource) = async {
            if cts.IsCancellationRequested then cts.Dispose() else msg |> receiver 
        }

        let stop msg receiver = async {
            do! msg |> receiver
        }

        let requestFetch = Agent.Start(fun inbox -> 
            let rec loop() = async {
                let! m = inbox.
                let! msg = inbox.Receive() 
                let cts = new CancellationTokenSource()
                match msg with 
                | FetchAtomFeed (receiver, msg:'a, url, cts) -> 
                    Async.StartImmediate(fetchAtomFeed msg receiver url cts), cts.Token
                    return! loop()
                
                | Stop (receiver, msg:'a) -> 
                    Async.StartImmediate(stop msg receiver), cts.Token
            }
            loop())
