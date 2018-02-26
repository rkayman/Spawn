namespace Amber.Spawn

module Courier = 

    open System
    open Utilities

    type MessageContext = {
        actorId: Guid;
        context: Map<string,Guid>;
    }

    type CourierResult = 
        | Delivered of MessageContext

    type CourierMessage<'pkg> = 
        | Send of 'pkg * CourierResult ResultMessage AsyncReplyChannel
        | Stop

    let defaultCourier id pkg = 
        printfn "[%s] %A" (DateTime.Now.ToString("yyyyMMddTHH:mm:ss.fffzzz")) pkg
        Success (Delivered { actorId = id; context = Map.empty })

    type CourierAgent<'pkg>(courier: Guid -> 'pkg -> CourierResult ResultMessage) =
        let agentId = Guid.NewGuid()
        let sender = courier
            
        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | Send (pkg, ch) -> 
                    pkg |> sender agentId |> ch.Reply
                    return! loop ()
                | Stop -> return! async.Zero()
            }
            loop ())
        
        new() = CourierAgent(defaultCourier)

        member __.Id with get() = agentId 

        member __.Send(package) =
            let buildMessage ch = Send (package, ch)
            agent.PostAndReply buildMessage
    

    module Kafka = 
        
        type FetchAtomFeed = {
             record: Configuration.RecordType;
             from: Configuration.String100;
             using: Uri;
        }

        let log pkg = 
            eprintfn "Fetching %A from '%A' using %A" pkg.record pkg.from pkg.using

        let kafkaCourier id pkg = 
            printfn "[%s] %A" (DateTime.Now.ToString("yyyyMMddTHH:mm:ss.fffzzz")) pkg
            Success (Delivered { actorId = id; context = Map.empty })

        type KafkaCourierAgent<'pkg>() = 
            inherit CourierAgent<'pkg>(kafkaCourier)
