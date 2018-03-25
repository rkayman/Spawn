namespace Amber.Spawn

module Utilities = 

    open System

    type Agent<'T> = MailboxProcessor<'T>

    let inline now () = DateTimeOffset.Now

    type IMessageContext =
        abstract MessageId: Guid
        abstract AgentId: Guid
        abstract ActivityId: Guid
        abstract CorrelationId: Guid
        abstract CausationId: Guid
