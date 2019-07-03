namespace Spawn.Clock

open NodaTime
open System


module Utilities =

    type TimeAdjusters = ToSecond | ToMinute | ToHour
    
    let now() = SystemClock.Instance.GetCurrentInstant()

    let toInstant (dt: ValueType) =
        match dt with
        | :? DateTimeOffset -> Instant.FromDateTimeOffset(dt :?> DateTimeOffset)
        | :? ZonedDateTime -> (dt :?> ZonedDateTime).ToInstant()
        | :? OffsetDateTime -> (dt :?> OffsetDateTime).ToInstant()
        | _ -> invalidArg "dt" "This type does not support conversion to NodaTime.Instant"

    let toDateTimeOffset (dt: ValueType) =
        match dt with
        | :? Instant -> (dt :?> Instant).ToDateTimeOffset()
        | :? ZonedDateTime -> (dt :?> ZonedDateTime).ToDateTimeOffset()
        | :? OffsetDateTime -> (dt :?> OffsetDateTime).ToDateTimeOffset()
        | _ -> invalidArg "dt" "This type does not support conversion System.DateTimeOffset"

    let toOffsetDateTime = OffsetDateTime.FromDateTimeOffset

    let private truncateTo adjuster (time: OffsetDateTime) = 
        match adjuster with
        | ToSecond  -> time.With(TimeAdjusters.TruncateToSecond)
        | ToMinute  -> time.With(TimeAdjusters.TruncateToMinute)
        | ToHour    -> time.With(TimeAdjusters.TruncateToHour)

    let normalizeTime timeAdjuster dateTimeOffset = 
        dateTimeOffset |> toOffsetDateTime |> truncateTo timeAdjuster |> toDateTimeOffset

    let toZonedDateTime = ZonedDateTime.FromDateTimeOffset

    let asLocalDateTime (dt: ZonedDateTime) = dt.LocalDateTime

    let asDate (dt: ZonedDateTime) = dt.Date

    let withAdjuster adjuster (dt: LocalDate) = adjuster |> dt.With

    let adjustWith (adjuster: LocalDateTime -> LocalDate) dateTime = dateTime |> adjuster

    let atTime time (dt: LocalDate) = dt.At(time)

    let inZone zone (ldt: LocalDateTime) = ldt.InZoneLeniently(zone)

    let withZone (zone: DateTimeZone) (dateTime: ZonedDateTime) = dateTime.WithZone(zone)

    let getZone name = DateTimeZoneProviders.Tzdb.GetZoneOrNull name |> Option.ofObj


module Time =
    
    type Rate =
        | Ticks        of int64
        | Milliseconds of int64
        | Seconds      of int64
        | Minutes      of int64
        | Hours        of int64
        | Days         of int64
        | Weeks        of int64
        | PerSecond    of int64
        | PerMinute    of int64
        | PerHour      of int64
        | PerDay       of int64
        
    let private computeRate rate period ticks =
        let r, t = decimal rate, decimal ticks
        Math.Round(period / r, 3) * t |> int64
        
    let private frequency = function
        | Ticks        t  -> t
        | Milliseconds ms -> ms * NodaConstants.TicksPerMillisecond
        | Seconds      s  -> s * NodaConstants.TicksPerSecond
        | Minutes      m  -> m * NodaConstants.TicksPerMinute
        | Hours        h  -> h * NodaConstants.TicksPerHour
        | Days         d  -> d * NodaConstants.TicksPerDay
        | Weeks        w  -> w * NodaConstants.TicksPerWeek
        | PerSecond    s  -> (s, 1000m, NodaConstants.TicksPerMillisecond) |||> computeRate
        | PerMinute    m  -> (m, 60m, NodaConstants.TicksPerSecond) |||> computeRate
        | PerHour      h  -> (h, 60m, NodaConstants.TicksPerMinute) |||> computeRate
        | PerDay       d  -> (d, 24m, NodaConstants.TicksPerHour) |||> computeRate
    
    type Rate with
        member this.Value with get() = frequency this
        member this.TimeSpan with get() = this |> (frequency >> TimeSpan.FromTicks)
    
    module Intervals =
        open Utilities
    
        type Interval =
            | Daily        of DailyInterval
            | Weekly       of WeeklyInterval
            | Fortnightly  of WeeklyInterval
            | SemiMonthly  of TwiceMonthlyInterval
            | Monthly      of MonthlyInterval
            | Quarterly    of AnnualInterval
            | SemiAnnually of AnnualInterval
            | Annually     of AnnualInterval
        and AlarmTime = { time: LocalTime; zone: DateTimeZone }
        and DailyInterval = { alarm: AlarmTime; kind: DailyKind }
        and WeeklyInterval = { alarm: AlarmTime; day: IsoDayOfWeek }
        and MonthlyInterval = { alarm: AlarmTime; modifier: DayAdjustmentStrategy; day: DayOfMonth }
        and TwiceMonthlyInterval = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                                     firstDay: DayOfMonth; secondDay: DayOfMonth }
        and AnnualInterval = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                               date: AnnualDate; adjustment: DateAdjustmentStrategy }
        and DailyKind = Everyday = 0 | Weekdays = 1 | Weekends = 2
        and DayAdjustmentStrategy = Rigid = 0 | WorkingDayBefore = 1 | WorkingDayClosest = 2
        and DateAdjustmentStrategy = Specific = 0 | Last = 1
        and DayOfMonth =
            | Day of int
            | Last
    
    
        let inline private validate day msg =
            if day = IsoDayOfWeek.None 
            then invalidArg "day" msg
    
        let private onOrBefore (date: LocalDate) =
            match date.DayOfWeek with
            | IsoDayOfWeek.Saturday -> date.PlusDays(-1)
            | IsoDayOfWeek.Sunday -> date.PlusDays(-2)
            | _ -> date
    
        let private onOrClosest (date: LocalDate) =
            match date.DayOfWeek with
            | IsoDayOfWeek.Saturday -> date.PlusDays(-1)
            | IsoDayOfWeek.Sunday -> date.PlusDays(1)
            | _ -> date
    
        let internal dayAdjuster = function
            | DayAdjustmentStrategy.Rigid -> id
            | DayAdjustmentStrategy.WorkingDayBefore -> onOrBefore
            | DayAdjustmentStrategy.WorkingDayClosest -> onOrClosest
            | _ -> invalidArg "modifier" "DayAdjustmentStrategy can be Rigid, WorkingDayBefore or WorkingDayClosest"
     
        let inline private fromFunc<'a> func = FuncConvert.FromFunc<'a, 'a> func
    
        let inline private revApply x f = f x
    
        let private dedup pattern =
            match pattern with
            | Daily { alarm = x } -> x
            | Weekly { alarm = x } | Fortnightly { alarm = x } -> x
            | SemiMonthly { alarm = x } -> x
            | Monthly { alarm = x } -> x
            | Quarterly { alarm = x } | SemiAnnually { alarm = x } | Annually { alarm = x } -> x
    
    
        module Daily =
    
            type private Template = 
                { nextIndices: int list * int list; prevIndices: int list * int list }
    
            let private everyday = { nextIndices = ([0;0;1;2;3;4;5;6], [0;1;2;3;4;5;6;0]);
                                     prevIndices = ([0;6;0;1;2;3;4;5], [0;0;1;2;3;4;5;6]) }
    
            let private weekdays = { nextIndices = ([0;0;1;2;3;4;0;0], [0;1;2;3;4;0;0;0]);
                                     prevIndices = ([4;0;1;2;3;4;4;4], [4;4;0;1;2;3;4;4]) }
    
            let private weekends = { nextIndices = ([5;5;5;5;5;5;5;6], [5;5;5;5;5;5;6;5]);
                                     prevIndices = ([6;6;6;6;6;6;5;6], [6;6;6;6;6;6;6;5]) }
    
            let private setup = function
                | DailyKind.Everyday -> everyday
                | DailyKind.Weekdays -> weekdays
                | DailyKind.Weekends -> weekends
                | _ -> invalidArg "kind" "Kind can be Everyday, Weekdays or Weekends"
    
            let private (>!<) adjuster =
                [1..7] |> Seq.cast<IsoDayOfWeek>
                       |> Seq.map (adjuster >> fromFunc)
                       |> Seq.toArray
    
            let private nextAdjusters = (>!<) DateAdjusters.NextOrSame
    
            let private prevAdjusters = (>!<) DateAdjusters.PreviousOrSame
    
            let inline private chooseIndex xs useSameDay (day: IsoDayOfWeek) = 
                if useSameDay then fst xs else snd xs
                |> List.item (int day)
        
            let private adjust op idxFunc adjusters (pattern: DailyInterval) (from: LocalDateTime) =
                let xs = setup pattern.kind |> idxFunc
                let useSameDay = op from.TimeOfDay pattern.alarm.time
                adjusters |> Array.item (chooseIndex xs useSameDay from.DayOfWeek)
                          |> revApply from.Date
    
            let private (<!!) = function { prevIndices = p } -> p
    
            let private (!!>) = function { nextIndices = n } -> n
    
            let next = adjust (<) (!!>) nextAdjusters
    
            let prev = adjust (>) (<!!) prevAdjusters
    
        module Weekly =
    
            let msg = "Day must be between Monday and Sunday for Weekly pattern"
    
            let private adjuster op dateAdjusters (pattern: WeeklyInterval) (from: LocalDateTime) =
                validate pattern.day msg
                pattern.day
                |> if op from.TimeOfDay pattern.alarm.time
                   then fst dateAdjusters
                   else snd dateAdjusters
                |> fromFunc |> revApply from.Date
    
            let next = adjuster (<) (DateAdjusters.NextOrSame, DateAdjusters.Next)
    
            let prev = adjuster (>) (DateAdjusters.PreviousOrSame, DateAdjusters.Previous)
    
        module Fortnightly =
    
            let private msg = "Day must be between Monday and Sunday for Fortnightly/BiWeekly pattern"
    
            let private fromFuncT day (f, g) =
                (fromFunc<LocalDate> (f(day)), fromFunc<LocalDate> (g(day)))
    
            let private adjuster op dateAdjusters (pattern: WeeklyInterval) (from: LocalDateTime) =
                validate pattern.day msg
                let f, g = dateAdjusters |> fromFuncT pattern.day
    
                from.Date
                |> if op from.TimeOfDay pattern.alarm.time
                   then g >> f
                   else f >> f
    
            let next = adjuster (<) (DateAdjusters.Next, DateAdjusters.NextOrSame)
    
            let prev = adjuster (>) (DateAdjusters.Previous, DateAdjusters.PreviousOrSame)
    
        module Period =
    
            type internal OffsetAdjustmentStrategy = Prev | Next
                
            let internal adjuster interval alarm (from: LocalDateTime) offset =
                let (!!) = function Prev -> (>), Seq.max | Next -> (<), Seq.min
                let op, comp = (!!) offset
                let withTime x = x |> atTime alarm.time |> inZone alarm.zone |> asLocalDateTime
                [-2; -1; 0; 1; 2]
                    |> Seq.map (fun offset -> from.Date |> interval offset |> withTime)
                    |> Seq.filter (fun dt -> op from dt)
                    |> comp
                    |> (fun x -> x.Date)
    
        module Monthly =
    
            open Period
    
            let inline private monthAdjuster offset (date: LocalDate) = date.PlusMonths(offset)
    
            let private dateAdjuster = function 
                | Last -> DateAdjusters.EndOfMonth |> fromFunc
                | Day n -> DateAdjusters.DayOfMonth(n) |> fromFunc
    
            let inline private monthly (pattern: MonthlyInterval) offset =
                monthAdjuster offset >> dateAdjuster pattern.day >> dayAdjuster pattern.modifier
            
            let next (pattern: MonthlyInterval) (from: LocalDateTime) = 
                Next |> Period.adjuster (monthly pattern) pattern.alarm from
    
            let prev (pattern: MonthlyInterval) (from: LocalDateTime) =
                Prev |> Period.adjuster (monthly pattern) pattern.alarm from
    
        module SemiMonthly =
    
            let private toAdjusted (adjuster: MonthlyInterval -> LocalDateTime -> LocalDate) pattern from =
                from |> adjuster pattern
                     |> atTime pattern.alarm.time
                     |> inZone pattern.alarm.zone
                     |> asLocalDateTime
    
            let private toMonthly (pattern: TwiceMonthlyInterval) =
                let msg = "FirstDay and SecondDay should be different, please check and change the values."
                let template = { alarm = pattern.alarm; modifier = pattern.modifier; day = pattern.firstDay }
                match compare pattern.firstDay pattern.secondDay with
                | 0 -> invalidArg "pattern.secondDay" msg
                | x when x < 0 -> template, { template with day = pattern.secondDay }
                | _            -> { template with day = pattern.secondDay }, template
    
            let private adjuster op periodFunc (pattern: TwiceMonthlyInterval) (from: LocalDateTime) =
                let first, second = toMonthly pattern
                let fval = from |> toAdjusted periodFunc first
                let sval = from |> toAdjusted periodFunc second
    
                let inline asInstant (dt: LocalDateTime) = dt.InUtc() |> toInstant
                let cmp op (v: LocalDateTime) f s = 
                    let v', f', s' = asInstant v, asInstant f, asInstant s
                    op (f' - v').TotalMilliseconds (s' - v').TotalMilliseconds
            
                if cmp op from fval sval then fval.Date else sval.Date
    
            let next = adjuster (<) Monthly.next
    
            let prev = adjuster (>) Monthly.prev
      
        module Annually =
    
            open Period
            
            let private dateAdjuster adjust (date: LocalDate) =
                if adjust = DateAdjustmentStrategy.Last then date.With(DateAdjusters.EndOfMonth) else date
    
            let inline private yearlyAdjuster offset (date: LocalDate) = date.PlusYears(offset)
    
            let inline private inYear (annual: AnnualDate) (date: LocalDate) = annual.InYear(date.Year)
    
            let inline private yearly (p: AnnualInterval) offset =
                inYear p.date >> yearlyAdjuster offset >> dateAdjuster p.adjustment >> dayAdjuster p.modifier
    
            let next (pattern: AnnualInterval) (from: LocalDateTime) = 
                Next |> Period.adjuster (yearly pattern) pattern.alarm from
    
            let prev (pattern: AnnualInterval) (from: LocalDateTime) =
                Prev |> Period.adjuster (yearly pattern) pattern.alarm from
    
        module Periodically =
    
            let private toAdjusted (adjuster: LocalDateTime -> LocalDate) alarm from =
                from |> adjuster |> atTime alarm.time |> inZone alarm.zone |> asLocalDateTime
                
            let private getAlarmTime = function { alarm = alarm; modifier = _; date = _; adjustment = _ } -> alarm
            
            let inline private mod12 x = (%) x 12
    
            let private toAnnualInterval is pattern cnt =
                let simpleDayCount = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]
                let m, d = pattern.date.Month, pattern.date.Day
                let i = 12 / is
                let x = m + (i * cnt) |> mod12
                let m' = if x = 0 then 12 else x
                let d' = if d <= simpleDayCount.[m'-1] then d else simpleDayCount.[m'-1]
                { pattern with date = AnnualDate(m', d') }
    
            let private getContext intervals pattern =
                match box pattern with
                | :? AnnualInterval as p -> toAnnualInterval intervals p, getAlarmTime p
                | _ -> failwith "Bad parameter. Expected Annual pattern."
    
            let internal adjuster op intervals periodAdjuster pattern (from: LocalDateTime) =
                let g, at = getContext intervals pattern
                let inline generator cnt = if cnt < intervals then Some (g cnt, cnt + 1) else None
                let inline adjust p = from |> toAdjusted (periodAdjuster p) at
                let inline asInstant (dt: LocalDateTime) = dt.InUtc() |> toInstant
                let diffFrom dt =
                    let dt', from' = asInstant dt, asInstant from
                    dt, (dt' - from').TotalMilliseconds
                let qs = 0 |> Seq.unfold generator |> Seq.map (adjust >> diffFrom)
                
                let inline comparer x y = if op (snd x) (snd y) then x else y
                let initial = Seq.head qs
                let asDate (x: LocalDateTime) = x.Date
                qs |> Seq.fold comparer initial |> fst |> asDate        
            
        module SemiAnnually =
    
            let private adjuster op periodFunc (pattern: AnnualInterval) (from: LocalDateTime) =
                Periodically.adjuster op 2 periodFunc pattern from
                
            let next = adjuster (<=) Annually.next
    
            let prev = adjuster (>) Annually.prev
    
        module Quarterly =
    
            let private adjuster op periodFunc (pattern: AnnualInterval) (from: LocalDateTime) =
                Periodically.adjuster op 4 periodFunc pattern from
    
            let next = adjuster (<) Annually.next
    
            let prev = adjuster (>) Annually.prev
    
    
        let private getAdjuster = function
            | Daily x -> (Daily.next x, Daily.prev x)
            | Weekly x -> (Weekly.next x, Weekly.prev x)
            | Fortnightly x -> (Fortnightly.next x, Fortnightly.prev x)
            | Monthly x -> (Monthly.next x, Monthly.prev x)
            | SemiMonthly x -> (SemiMonthly.next x, SemiMonthly.prev x)
            | Quarterly x -> (Quarterly.next x, Quarterly.prev x)
            | SemiAnnually x -> (SemiAnnually.next x, SemiAnnually.prev x)
            | Annually x -> (Annually.next x, Annually.prev x)
    
        let private adjust chooser (interval: Interval) (instant: Instant) =
            let { time = time; zone = zone } = dedup interval
            let zdt = instant |> toDateTimeOffset |> toZonedDateTime |> withZone (zone)
            let adjuster = interval |> getAdjuster |> chooser
    
            zdt |> asLocalDateTime
                |> adjustWith adjuster
                |> atTime time
                |> inZone zone
                |> toInstant
    
        let next = adjust fst
    
        let prev = adjust snd
