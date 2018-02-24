namespace Amber.Spawn

module Utilities = 

    type Agent<'T> = MailboxProcessor<'T>

    type ResultMessage<'a> = 
        | Success of 'a
        | Error of string 
