namespace Spawn.Clock

open NodaTime
open FSharp.Control.Reactive
open System.Reactive
open System.Reactive.Concurrency
open System
open Spawn.Clock.Time

module BalanceWheel =
    
    [<Struct>]
    type Recoil = 
        { Count: int64; Timestamp: Instant } with
        override this.ToString() = 
            let instant = this.Timestamp.ToDateTimeOffset().ToString("HH:mm:ss.fff zzz")
            sprintf "Count = %03i; Timestamp = %s" this.Count instant

    let inline private (!!) x = Instant.FromDateTimeOffset(x)
    
    let inline private (!>) (x: Timestamped<int64>) = 
        { Count = x.Value; Timestamp = !! x.Timestamp }

    let private startInternal interval timestamp recoilRate =
        recoilRate |> interval
                   |> timestamp
                   |> Observable.map (!>)
                   |> Observable.publish
                   |> Observable.refCount

    let start recoilRate =
        let interval = Observable.intervalOn (NewThreadScheduler.Default)
        let timestamp = Observable.timestamp
        startInternal interval timestamp recoilRate

    let startOn recoilRate (scheduler: #IScheduler) =
        let interval = Observable.intervalOn scheduler
        let timestamp = Observable.timestampOn scheduler
        startInternal interval timestamp recoilRate


module Repeater =

    open BalanceWheel

    [<Struct>]
    type PetitSonnerie = 
        { Time: Instant; Recoil: int64 } with
        override this.ToString() = 
            let instant = this.Time.ToDateTimeOffset().ToString("HH:mm:ss.fff zzz")
            sprintf "Recoils = %03i; StrikeTime = %s" this.Recoil instant


    let internal strikeNever : IObservable<PetitSonnerie> = Observable.infinite ()


    let inline private (!>) x = { Time = x.Timestamp; Recoil = x.Count }

    let private skipUntil skipFunc (now: DateTimeOffset) = function
        | Choice1Of2 dto -> dto |> skipFunc
        | Choice2Of2 ts -> now.Add(ts) |> skipFunc

    let private strikeOnceInternal skipUntil source =
        source |> skipUntil
               |> Observable.map (!>)
               |> Observable.take 1

    let internal strikeOnce strikeTime source =
        let skipUntil = strikeTime |> skipUntil (Observable.skipUntil) (DateTimeOffset.Now)
        strikeOnceInternal skipUntil source

    let internal strikeOnceOn strikeTime scheduler source =
        let skipUntil = strikeTime |> skipUntil (Observable.skipUntilOn scheduler) (scheduler.Now)
        strikeOnceInternal skipUntil source
    

    let private compare nextInterval (last: PetitSonnerie * Instant) cur =
        let next = snd last
        if cur.Timestamp < next then last
        else let next' = next |> nextInterval
             (!>) cur, next'

    let private strikeEveryInternal observe (period: TimeSpan) last source =
        let nextInterval (i: Instant) = i.PlusTicks(period.Ticks)
        let instant = Instant.FromDateTimeOffset(last) |> nextInterval
        let seed = ({ Time = instant; Recoil = 0L}, instant)
        let ignoreFirst = (<>) seed
        source |> observe
               |> Observable.scanInit seed (compare nextInterval)
               |> Observable.filter ignoreFirst
               |> Observable.map fst
               |> Observable.distinctUntilChangedKey (fun x -> x.Recoil)

    let internal strikeEvery period last source = strikeEveryInternal (id) period last source

    let internal strikeEveryOn period last scheduler source =
        let observe = Observable.observeOn scheduler
        strikeEveryInternal observe period last source


    let compareReplace nextInterval (last: PetitSonnerie * Instant) cur =
        let next = snd last
        if cur.Timestamp < next then last
        else let next' = cur.Timestamp |> nextInterval
             (!>) cur, next'
        
    let private strikeInternal observe pattern last source =
        let nextInterval = Intervals.next pattern
        let next = last |> nextInterval
        let seed = ({ Time = last; Recoil = 0L}, next)
        let ignoreFirst = (<>) seed
        source |> observe
               |> Observable.scanInit seed (compareReplace nextInterval)
               |> Observable.filter ignoreFirst
               |> Observable.map fst
               |> Observable.distinctUntilChangedKey (fun x -> x.Recoil)

    let internal strike pattern last source = strikeInternal (id) pattern last source

    let internal strikeOn pattern last scheduler source =
        let observe = Observable.observeOn scheduler
        strikeInternal observe pattern last source


module Alarm =
    
    open Spawn.Clock.Time.Intervals

    type Configuration = 
    | OnceAt of DateTimeOffset
    | OnceAfter of TimeSpan
    | Every of RepeatPattern * DateTimeOffset option
    | Schedule of AlarmInterval * Instant option
    | Never
    and RepeatPattern =
    | Frequency of Frequency
    | Rate of Rate
    | Interval of TimeSpan
    
    let private defRecoilRate = (Ms 333L).TimeSpan

    let private recoils = BalanceWheel.start defRecoilRate

    let private recoilsOn (scheduler: #IScheduler) rate = 
        BalanceWheel.startOn (rate |> Option.defaultValue defRecoilRate) scheduler

    let normalize (span: TimeSpan) (source: DateTimeOffset) =
        let s = if span.Minutes > 0 then 0 else source.Second
        let m = if span.Hours > 0 then 0 else source.Minute
        DateTimeOffset(source.Year, source.Month, source.Day, source.Hour, m, s, 0, source.Offset)

    let asTimeSpan = function
        | Interval x -> x
        | Rate x -> x.TimeSpan
        | Frequency x -> x.TimeSpan

    let asTicks = function
        | Interval x -> x.Ticks
        | Rate x -> x.Value
        | Frequency x -> x.Value

    type RepeatPattern with
        member this.TimeSpan with get() = asTimeSpan this
        member this.Ticks with get() = asTicks this

    let schedule config =
        match config with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnce (Choice1Of2 due) recoils
        | OnceAfter delay -> Repeater.strikeOnce (Choice2Of2 delay) recoils
        | Every (period, startAt) -> 
            let period' = period.TimeSpan
            let defaultStart = DateTimeOffset.Now.Subtract(period')
            let startAt' = startAt |> Option.defaultValue defaultStart
            recoils |> Observable.skipUntil (normalize period' startAt')
                    |> Repeater.strikeEvery period' startAt'
        | Schedule (pattern, last) ->
            let instant = Instant.FromDateTimeOffset(DateTimeOffset.Now)
            let defaultStart = prev pattern instant
            let last' = last |> Option.defaultValue defaultStart
            recoils |> Repeater.strike pattern last'

    let scheduleOn config scheduler recoilRate =
        let source = recoilsOn scheduler recoilRate
        match config with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnceOn (Choice1Of2 due) scheduler source
        | OnceAfter delay -> Repeater.strikeOnceOn (Choice2Of2 delay) scheduler source
        | Every (period, startAt) -> 
            let period' = period.TimeSpan
            let defaultStart = if scheduler.Now = DateTimeOffset.MinValue ||
                                  scheduler.Now - DateTimeOffset.MinValue < period' 
                               then scheduler.Now
                               else scheduler.Now.Subtract(period')
            let startAt' = startAt |> Option.defaultValue defaultStart
            source |> Observable.skipUntilOn scheduler startAt'
                   |> Repeater.strikeEveryOn period' startAt' scheduler
        | Schedule (pattern, last) ->
            let defaultStart = Instant.FromDateTimeOffset(scheduler.Now)
            let last' = last |> Option.defaultValue defaultStart
            source |> Repeater.strikeOn pattern last' scheduler
