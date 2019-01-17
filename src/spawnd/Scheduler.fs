namespace Spawn

open Spawn.Clock.Time
open Spawn.Clock.Time.Intervals
open System

module Scheduler =
    
     type Alarm<'a> = {
         id: Guid
         name: string
         schedule: Schedule
         payload: 'a

     } and Schedule =
         | Frequency of Frequency
         | Rate of Rate
         | Interval of AlarmInterval
     
     
