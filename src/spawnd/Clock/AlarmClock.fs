namespace Spawn.Clock

open NodaTime
open FSharp.Control.Reactive
open Spawn.IO.Configuration
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

    let startOn (scheduler: #IScheduler) recoilRate =
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

    let internal strikeOnceOn scheduler strikeTime source =
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

    let internal strikeEveryOn scheduler period last source =
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

    let internal strikeOn scheduler pattern last source =
        let observe = Observable.observeOn scheduler
        strikeInternal observe pattern last source


module Alarm =
    open Utilities
    open Time.Intervals

    type Recurrence = 
        | OnceAt of DateTimeOffset
        | OnceAfter of TimeSpan
        | Every of Frequency * After
        | Repeat of Schedule * Starting
        | Never
    and After = After of DateTimeOffset option
    and Starting = Starting of Instant option
    
    let private defRecoilRate = { size = 333L; unit = UnitOfTime.Milliseconds } |> toTimeSpan

    let private recoils = BalanceWheel.start defRecoilRate

    let private recoilsOn (scheduler: #IScheduler) rate = 
        BalanceWheel.startOn scheduler (rate |> Option.defaultValue defRecoilRate)

    let normalize (span: TimeSpan) (source: DateTimeOffset) =
        let s = if span.Minutes > 0 then 0 else source.Second
        let m = if span.Hours > 0 then 0 else source.Minute
        DateTimeOffset(source.Year, source.Month, source.Day, source.Hour, m, s, 0, source.Offset)

    let private frequencyRepeaterInternal skipUntil strikeEvery due freq source =
        let freqTS = freq |> toTimeSpan
        let defaultStart = DateTimeOffset.Now.Subtract(freqTS)
        let due' = due |> Option.defaultValue defaultStart        
        source |> skipUntil (normalize freqTS due')
               |> strikeEvery freqTS due'
        
    let private frequencyRepeater due freq source =
        frequencyRepeaterInternal (Observable.skipUntil) (Repeater.strikeEvery) due freq source

    let private frequencyRepeaterOn scheduler due freq source =
        frequencyRepeaterInternal (Observable.skipUntilOn scheduler) (Repeater.strikeEveryOn scheduler) due freq source

    let schedule recurrence =
        match recurrence with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnce (Choice1Of2 due) recoils
        | OnceAfter delay -> Repeater.strikeOnce (Choice2Of2 delay) recoils
        | Every (freq, After delay) -> recoils |> frequencyRepeater delay freq
        | Repeat (timeTable, Starting latest) ->
            match timeTable with
            | Frequency freq ->
                let delay = latest |> Option.map toDateTimeOffset
                recoils |> frequencyRepeater delay freq
            | _ -> 
                let instant = now()
                let defaultStart = prev timeTable instant
                let last = latest |> Option.defaultValue defaultStart
                recoils |> Repeater.strike timeTable last

    let scheduleOn scheduler recoilRate config =
        let source = recoilsOn scheduler recoilRate
        match config with
        | Never -> Repeater.strikeNever
        | OnceAt due -> Repeater.strikeOnceOn scheduler (Choice1Of2 due) source
        | OnceAfter delay -> Repeater.strikeOnceOn scheduler (Choice2Of2 delay) source
        | Every (freq, After delay) -> source |> frequencyRepeaterOn scheduler delay freq
        | Repeat (timeTable, Starting latest) ->
            match timeTable with
            | Frequency freq ->
                let delay = latest |> Option.map toDateTimeOffset
                source |> frequencyRepeaterOn scheduler delay freq
            | _ ->
                let defaultStart = Instant.FromDateTimeOffset(scheduler.Now)
                let last = latest |> Option.defaultValue defaultStart
                source |> Repeater.strikeOn scheduler timeTable last
