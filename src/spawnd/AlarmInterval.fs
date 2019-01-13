namespace Spawn

open NodaTime
open NodaUtilities

module AlarmInterval =

    type AlarmPattern =
        | Daily of DailyPattern
        | Weekly of WeeklyPattern
        | Fortnightly of WeeklyPattern
        | BiWeekly of WeeklyPattern
        | SemiMonthly of TwiceMonthlyPattern
        | Monthly of MonthlyPattern
        | Quarterly of AnnualPattern
        | SemiAnnually of AnnualPattern
        | Annually of AnnualPattern
    and AlarmTime = { time: LocalTime; zone: DateTimeZone }
    and DailyPattern = { alarm: AlarmTime; kind: DailyKind }
    and WeeklyPattern = { alarm: AlarmTime; day: IsoDayOfWeek }
    and MonthlyPattern = { alarm: AlarmTime; modifier: DayAdjustmentStrategy; day: DayOfMonth }
    and TwiceMonthlyPattern = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                                firstDay: DayOfMonth; secondDay: DayOfMonth }
    and AnnualPattern = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                          date: AnnualDate; adjustment: DateAdjustmentStrategy }
    and DayAdjustmentStrategy = Rigid | WorkingDayBefore | WorkingDayClosest
    and DateAdjustmentStrategy = Specific | Last
    and DailyKind = Everyday | Weekdays | Weekends
    and DayOfMonth =
        | First         = 1
        | Second        = 2
        | Third         = 3
        | Fourth        = 4
        | Fifth         = 5
        | Sixth         = 6
        | Seventh       = 7
        | Eighth        = 8
        | Nineth        = 9
        | Tenth         = 10
        | Eleventh      = 11
        | Twelfth       = 12
        | Thirteenth    = 13
        | Fourteenth    = 14
        | Fifteenth     = 15
        | Sixteenth     = 16
        | Seventeenth   = 17
        | Eighteenth    = 18
        | Nineteenth    = 19
        | Twentieth     = 20
        | TwentyFirst   = 21
        | TwentySecond  = 22
        | TwentyThird   = 23
        | TwentyFourth  = 24
        | TwentyFifth   = 25
        | TwentySixth   = 26
        | TwentySeventh = 27
        | TwentyEighth  = 28
        | Last          = 32


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
        | Rigid -> id
        | WorkingDayBefore -> onOrBefore
        | WorkingDayClosest -> onOrClosest
 
    let inline private fromFunc<'a> func = FuncConvert.FromFunc<'a, 'a> func

    let inline private revApply x f = f x

    let private dedup pattern =
        match pattern with
        | Daily { alarm = x } -> x
        | Weekly { alarm = x } | Fortnightly { alarm = x } | BiWeekly { alarm = x } -> x
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
            | Everyday -> everyday
            | Weekdays -> weekdays
            | Weekends -> weekends

        let private (>!<) adjuster =
            [1..7] |> Seq.cast<IsoDayOfWeek>
                   |> Seq.map (adjuster >> fromFunc)
                   |> Seq.toArray

        let private nextAdjusters = (>!<) DateAdjusters.NextOrSame

        let private prevAdjusters = (>!<) DateAdjusters.PreviousOrSame

        let inline private chooseIndex xs useSameDay (day: IsoDayOfWeek) = 
            if useSameDay then fst xs else snd xs
            |> List.item (int day)
    
        let private adjust op idxFunc adjusters (pattern: DailyPattern) (from: LocalDateTime) =
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

        let private adjuster op dateAdjusters (pattern: WeeklyPattern) (from: LocalDateTime) =
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

        let private adjuster op dateAdjusters (pattern: WeeklyPattern) (from: LocalDateTime) =
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
            | DayOfMonth.Last -> DateAdjusters.EndOfMonth |> fromFunc
            | day -> DateAdjusters.DayOfMonth(int day) |> fromFunc

        let inline private monthly (pattern: MonthlyPattern) offset =
            monthAdjuster offset >> dateAdjuster pattern.day >> dayAdjuster pattern.modifier
        
        let next (pattern: MonthlyPattern) (from: LocalDateTime) = 
            Next |> Period.adjuster (monthly pattern) pattern.alarm from

        let prev (pattern: MonthlyPattern) (from: LocalDateTime) =
            Prev |> Period.adjuster (monthly pattern) pattern.alarm from

    module SemiMonthly =

        let private toAdjusted (adjuster: MonthlyPattern -> LocalDateTime -> LocalDate) pattern from =
            from |> adjuster pattern
                 |> atTime pattern.alarm.time
                 |> inZone pattern.alarm.zone
                 |> asLocalDateTime

        let private toMonthly (pattern: TwiceMonthlyPattern) =
            let msg = "FirstDay and SecondDay should be different, please check and change the values."
            let template = { alarm = pattern.alarm; modifier = pattern.modifier; day = pattern.firstDay }
            match compare pattern.firstDay pattern.secondDay with
            | 0 -> invalidArg "pattern.secondDay" msg
            | x when x < 0 -> template, { template with day = pattern.secondDay }
            | _            -> { template with day = pattern.secondDay }, template

        let private adjuster op periodFunc (pattern: TwiceMonthlyPattern) (from: LocalDateTime) =
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
            if adjust = Last then date.With(DateAdjusters.EndOfMonth) else date

        let inline private yearlyAdjuster offset (date: LocalDate) = date.PlusYears(offset)

        let inline private inYear (annual: AnnualDate) (date: LocalDate) = annual.InYear(date.Year)

        let inline private yearly (p: AnnualPattern) offset =
            inYear p.date >> yearlyAdjuster offset >> dateAdjuster p.adjustment >> dayAdjuster p.modifier

        let next (pattern: AnnualPattern) (from: LocalDateTime) = 
            Next |> Period.adjuster (yearly pattern) pattern.alarm from

        let prev (pattern: AnnualPattern) (from: LocalDateTime) =
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
            | :? AnnualPattern as p -> toAnnualInterval intervals p, getAlarmTime p
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

        let private adjuster op periodFunc (pattern: AnnualPattern) (from: LocalDateTime) =
            Periodically.adjuster op 2 periodFunc pattern from
            
        let next = adjuster (<=) Annually.next

        let prev = adjuster (>) Annually.prev

    module Quarterly =

        let private adjuster op periodFunc (pattern: AnnualPattern) (from: LocalDateTime) =
            Periodically.adjuster op 4 periodFunc pattern from

        let next = adjuster (<) Annually.next

        let prev = adjuster (>) Annually.prev


    let private getAdjuster = function
        | Daily x -> (Daily.next x, Daily.prev x)
        | Weekly x -> (Weekly.next x, Weekly.prev x)
        | Fortnightly x | BiWeekly x -> (Fortnightly.next x, Fortnightly.prev x)
        | Monthly x -> (Monthly.next x, Monthly.prev x)
        | SemiMonthly x -> (SemiMonthly.next x, SemiMonthly.prev x)
        | Quarterly x -> (Quarterly.next x, Quarterly.prev x)
        | SemiAnnually x -> (SemiAnnually.next x, SemiAnnually.prev x)
        | Annually x -> (Annually.next x, Annually.prev x)

    let private adjust chooser (pattern: AlarmPattern) (instant: Instant) =
        let { time = time; zone = zone } = dedup pattern
        let zdt = instant |> toDateTimeOffset |> toZonedDateTime |> withZone (zone)
        let adjuster = pattern |> getAdjuster |> chooser

        zdt |> asLocalDateTime
            |> adjustWith adjuster
            |> atTime time
            |> inZone zone
            |> toInstant

    let next = adjust fst

    let prev = adjust snd
