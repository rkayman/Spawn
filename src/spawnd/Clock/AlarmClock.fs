namespace Spawn.Clock

open NodaTime
open FSharp.Control.Reactive
open System.Reactive
open System.Reactive.Concurrency
open System
open Time

module BalanceWheel =
    
    [<Struct>]
    type Recoil = 
        { Count: int64; Timestamp: Instant } with
        override this.ToString() = 
            let instant = this.Timestamp.ToDateTimeOffset().ToString("HH:mm:ss.fff zzz")
            sprintf "Count = %03i; Timestamp = %s" this.Count instant
    
    let inline private (!>) (x: Timestamped<int64>) =
        { Count = x.Value; Timestamp = Instant.FromDateTimeOffset(x.Timestamp) }

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
    open Utilities
    open Time.Intervals

    type Occurrence = 
        | OnceAt of DateTimeOffset
        | OnceAfter of TimeSpan
        | Every of Rate * After
        | Repeat of Interval * Starting
        | Never
    and After = After of DateTimeOffset option
    and Starting = Starting of Instant option
    
    let private defRecoilRate = (Milliseconds 333L).TimeSpan

    let private recoils = BalanceWheel.start defRecoilRate

    let private recoilsOn (scheduler: #IScheduler) rate = 
        BalanceWheel.startOn (rate |> Option.defaultValue defRecoilRate) scheduler

    let normalize (span: TimeSpan) (source: DateTimeOffset) =
        let s = if span.Minutes > 0 then 0 else source.Second
        let m = if span.Hours > 0 then 0 else source.Minute
        DateTimeOffset(source.Year, source.Month, source.Day, source.Hour, m, s, 0, source.Offset)

    let schedule config =
        match config with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnce (Choice1Of2 due) recoils
        | OnceAfter delay -> Repeater.strikeOnce (Choice2Of2 delay) recoils
        | Every (rate, After delay) ->
            let rate' = rate.TimeSpan
            let defaultStart = DateTimeOffset.Now.Subtract(rate')
            let delay' = delay |> Option.defaultValue defaultStart
            recoils |> Observable.skipUntil (normalize rate' delay')
                    |> Repeater.strikeEvery rate' delay'
        | Repeat (interval, Starting latest) ->
            let instant = now()
            let defaultStart = prev interval instant
            let last = latest |> Option.defaultValue defaultStart
            recoils |> Repeater.strike interval last

    let scheduleOn config scheduler recoilRate =
        let source = recoilsOn scheduler recoilRate
        match config with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnceOn (Choice1Of2 due) scheduler source
        | OnceAfter delay -> Repeater.strikeOnceOn (Choice2Of2 delay) scheduler source
        | Every (rate, After delay) ->
            let rate' = rate.TimeSpan
            let defaultStart = if scheduler.Now = DateTimeOffset.MinValue ||
                                  scheduler.Now - DateTimeOffset.MinValue < rate'
                               then scheduler.Now
                               else scheduler.Now.Subtract(rate')
            let delay' = delay |> Option.defaultValue defaultStart
            source |> Observable.skipUntilOn scheduler delay'
                   |> Repeater.strikeEveryOn rate' delay' scheduler
        | Repeat (interval, Starting latest) ->
            let defaultStart = Instant.FromDateTimeOffset(scheduler.Now)
            let last = latest |> Option.defaultValue defaultStart
            source |> Repeater.strikeOn interval last scheduler
