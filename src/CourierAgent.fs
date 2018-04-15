namespace Amber.Spawn

module Courier = 

    open System
    open Utilities

    type PackageInfo<'T> = {
        agentId: Guid;
        messageId: Guid;
        activityId: Guid;
        correlationId: Guid;
        causationId: Guid;
        payload: 'T;
    } with 
        member this.GetMessageContext() = 
            { new IMessageContext with
                member __.AgentId with get() = this.agentId
                member __.MessageId with get() = this.messageId
                member __.ActivityId with get() = this.activityId
                member __.CausationId with get() = this.causationId
                member __.CorrelationId with get() = this.correlationId }

    type DeliveryInfo<'T> = {
        agentId: Guid;
        messageId: Guid;
        activityId: Guid;
        correlationId: Guid;
        causationId: Guid;
        shippedAt: DateTimeOffset;
        message: string;
        payload: 'T;
    } with
        member this.GetMessageContext() = 
            { new IMessageContext with
                member __.AgentId with get() = this.agentId
                member __.MessageId with get() = this.messageId
                member __.ActivityId with get() = this.activityId
                member __.CausationId with get() = this.causationId
                member __.CorrelationId with get() = this.correlationId }

    type CourierResult<'T> =
        | Shipped of DateTimeOffset * DeliveryInfo<'T>
        | Stopped of DateTimeOffset * string
        | Error of DateTimeOffset * string * Exception option

    type CourierMessage<'T> =
        | Ship of PackageInfo<'T> * 'T CourierResult AsyncReplyChannel
        | Stop of string CourierResult AsyncReplyChannel

    let private formatTime (time: DateTimeOffset) = time.ToString("yyyyMMddTHH:mm:ss.fffzzz")

    let private deliverPackage agentId shipTime msg (pkg: PackageInfo<'T>) = 
        { agentId = agentId;
          messageId = Guid.NewGuid();
          activityId = pkg.activityId;
          causationId = pkg.causationId;
          correlationId = pkg.correlationId;
          shippedAt = shipTime;
          message = msg;
          payload = pkg.payload }

    let private defaultCourier agentId (package: PackageInfo<'T>) = 
        let deliveryTime = DateTimeOffset.Now
        printfn "[%s] %A" (formatTime deliveryTime) package.payload
        let msg = sprintf "Delivered at: %s" (formatTime deliveryTime)
        Shipped (now(), package |> deliverPackage agentId deliveryTime msg)

    type CourierAgent<'T>(courier: Guid -> PackageInfo<'T> -> CourierResult<_>) =
        
        let agentId = Guid.NewGuid()
        let ship = courier agentId
            
        let agent = Agent.Start(fun inbox -> 
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | Ship (package, channel) -> 
                    package |> ship |> channel.Reply
                    return! loop ()

                | Stop channel -> 
                    let stoppedAt = DateTimeOffset.Now
                    let msg = sprintf "Stopped at: %s with %i messages remaining in inbox" 
                                (formatTime stoppedAt) inbox.CurrentQueueLength
                    channel.Reply (Stopped (stoppedAt, msg))
                    return! async.Zero()
            }
            loop ())
        
        new() = CourierAgent(defaultCourier)

        member __.Id with get() = agentId 

        member __.Ship(package) =
            let buildMessage ch = Ship (package, ch)
            agent.PostAndReply buildMessage

        member __.Stop() =
            agent.PostAndReply (Stop)
    

    module Kafka = 

        open Kafunk

        let private conn host = async {
            return! Kafka.connHostAsync host
        }

        let private makeConfig topic = Kafunk.ProducerConfig.create
                                        ( topic, 
                                          Partitioner.roundRobin, 
                                          RequiredAcks.Local )

        let private producer host topic = async {
            let! kc = conn host
            return! Producer.createAsync kc (makeConfig topic)
        }

        let private produce host topic msg = async {
            let! p = producer host topic
            let str = sprintf "%A" msg
            return! Producer.produce p (ProducerMessage.ofString str)
        }

        let kafkaCourierAsync host topic agentId (package: PackageInfo<'T>) = async {

            let deliveryTime = DateTimeOffset.Now
            let! result = package.payload |> produce host topic
            let msg = sprintf "Delivered at: %s on partition = %i at offset = %i"
                        (formatTime deliveryTime)
                        result.partition
                        result.offset
            return Shipped (now(), package |> deliverPackage agentId deliveryTime msg)
        }

        let kafkaCourier host topic agentId (package: PackageInfo<'T>) =
            kafkaCourierAsync host topic agentId package |> Async.RunSynchronously

        type KafkaCourierAgent<'T>(host, topic) =
            inherit CourierAgent<'T>(kafkaCourier host topic)

            new() = KafkaCourierAgent("localhost:29092", "test")
