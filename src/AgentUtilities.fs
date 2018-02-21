namespace Amber.Spawn

module Utilities = 

    type ResultMessage<'a> = 
        | Success of 'a
        | Error of string 