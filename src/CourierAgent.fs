namespace Amber.Spawn

module Courier = 

    open System
    open Utilities

    type PackageInfo<'T> = {
        courierId: Guid;
        activityId: Guid;
        correlationId: Guid;
        causationId: Guid;
        payload: 'T;
    } with 
        member this.GetMessageContext() = 
            { new IMessageContext with
                member __.AgentId with get() = this.courierId
                member __.ActivityId with get() = this.activityId
                member __.CausationId with get() = this.causationId
                member __.CorrelationId with get() = this.correlationId }

    type DeliveryInfo<'T> = {
        courierId: Guid;
        activityId: Guid;
        correlationId: Guid;
        causationId: Guid;
        shippedAt: DateTimeOffset;
        message: string;
        payload: 'T;
    } with
        member this.GetMessageContext() = 
            { new IMessageContext with
                member __.AgentId with get() = this.courierId
                member __.ActivityId with get() = this.activityId
                member __.CausationId with get() = this.causationId
                member __.CorrelationId with get() = this.correlationId }

    type CourierResult<'T> =
        | Shipped of DeliveryInfo<'T>
        | Stopped of DateTimeOffset * string
        | Error of string * Exception option

    type CourierMessage<'T> =
        | Ship of PackageInfo<'T> * 'T CourierResult AsyncReplyChannel
        | Stop of string CourierResult AsyncReplyChannel

    let formatTime (time: DateTimeOffset) = time.ToString("yyyyMMddTHH:mm:ss.fffzzz")

    let deliverPackage agentId shipTime msg (pkg: PackageInfo<'T>) = 
        { courierId = agentId;
          activityId = pkg.activityId;
          causationId = pkg.causationId;
          correlationId = pkg.correlationId;
          shippedAt = shipTime;
          message = msg;
          payload = pkg.payload }

    let defaultCourier agentId (package: PackageInfo<'T>) = 
        let deliveryTime = DateTimeOffset.Now
        printfn "[%s] %A" (formatTime deliveryTime) package.payload
        let msg = sprintf "Delivered at: %s" (formatTime deliveryTime)
        Shipped (package |> deliverPackage agentId deliveryTime msg)

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
    

    module Kafka = 

        let kafkaCourier agentId (package: PackageInfo<'T>) = 
            let deliveryTime = DateTimeOffset.Now
            printfn "[%s] %A" (formatTime deliveryTime) package.payload
            let msg = sprintf "Delivered at: %s" (formatTime deliveryTime)
            Shipped (package |> deliverPackage agentId deliveryTime msg)

        type KafkaCourierAgent<'T>() =
            inherit CourierAgent<'T>(kafkaCourier)
