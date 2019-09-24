namespace Spawn

module Messaging =
    open NodaTime
    open System
    open Clock.Utilities
    
    type Agent<'a> = MailboxProcessor<'a>

    type Message<'a> = {
        id: Guid
        activity: Guid
        causation: Guid
        correlation: Guid
        timestamp: Instant
        name: string
        body: 'a
    }

    let newMessage a n b =
        let guid = Guid.NewGuid()
        { id = guid
          activity = a
          causation = guid
          correlation = guid
          timestamp = now()
          name = n
          body = b }

    let nextMessage (evt : Message<_>) n b =
        let nxt = newMessage evt.activity n b
        { nxt with causation = evt.id
                   correlation = evt.correlation }

