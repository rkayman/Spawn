namespace Amber.Spawn

module Utilities = 

    open System

    type Agent<'T> = MailboxProcessor<'T>

    type IMessageContext =
        abstract AgentId: Guid
        abstract ActivityId: Guid
        abstract CorrelationId: Guid
        abstract CausationId: Guid

    type IMessageContext<'a> =
        abstract ActorId: Guid
        abstract ActivityId: Guid
        abstract CorrelationId: Guid
        abstract CausationId: Guid
        abstract Content: 'a

    type MessageContext<'a> = { 
        id: Guid;
        activityId: Guid;
        correlationId: Guid;
        causationId: Guid;
        content: 'a;
    } with 
        member this.Id with get() = this.id
        member this.ActivityId with get() = this.activityId
        member this.CorrelationId with get() = this.correlationId
        member this.CausationId with get() = this.causationId
        member this.Content with get() = this.content
        static member Create(agent: Guid, content: 'a) =
            { id = agent;
              activityId = Guid.Empty;
              correlationId = Guid.Empty;
              causationId = Guid.Empty;
              content = content }
