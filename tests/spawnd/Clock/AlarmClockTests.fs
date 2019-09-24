module Tests.Spawn.Clock.ClockTests

open Spawn.Clock
open Spawn.Clock.Alarm
open Spawn.Clock.Repeater
open Spawn.Configuration
open Spawn.Clock.Time
open NodaTime
open Xunit
open FsUnit
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Testing
open Microsoft.Reactive.Testing
open System.Reactive.Concurrency
open System


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

let private makeFrequency size unit = { size = size; unit = unit }

let private toFrequency = function
    | Ticks x        -> makeFrequency x UnitOfTime.Ticks
    | Milliseconds x -> makeFrequency x UnitOfTime.Milliseconds
    | Seconds x      -> makeFrequency x UnitOfTime.Seconds
    | Minutes x      -> makeFrequency x UnitOfTime.Minutes
    | Hours x        -> makeFrequency x UnitOfTime.Hours
    | Days x         -> makeFrequency x UnitOfTime.Days
    | Weeks x        -> makeFrequency x UnitOfTime.Weeks
    | PerSecond x    -> makeFrequency x UnitOfTime.PerSecond
    | PerMinute x    -> makeFrequency x UnitOfTime.PerMinute
    | PerHour x      -> makeFrequency x UnitOfTime.PerHour
    | PerDay x       -> makeFrequency x UnitOfTime.PerDay
    
type Rate with
    member x.Value with get() = toFrequency x |> rate
    member x.TimeSpan with get() = x |> toFrequency |> toTimeSpan

let private oneSec = Seconds 1L
let private oneMin = Minutes 1L


module BalanceWheelTests =

    [<Fact>]
    let ``BalanceWheel coils at fixed frequency`` () =
        let freq = Ticks 100L
        let subscriptionOffset = Ticks 200L

        let timeOffset x = subscriptionOffset.Value + ((x + 1L) * freq.Value)
        let makeNotification x = TestNotification.onNext (timeOffset x) x

        let expected = [0L..6L] |> List.map makeNotification

        let sched = TestScheduler()
        let hb = BalanceWheel.startOn sched freq.TimeSpan
                 |> Observable.map (fun x -> x.Count)

        let actual = TestSchedule.startObservable sched hb 
                     |> TestObserver.messages
                     |> TestNotification.unwrap
        
        actual |> should haveLength expected.Length
        actual |> List.iteri (fun i x -> x |> should equal (expected.[i]))

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(4)>]
    [<InlineData(10)>]
    let ``BalanceWheel coils at given rate per second`` num =
        let sched = HistoricalScheduler(DateTimeOffset.Now)
        let rate = PerSecond (int64 num)

        BalanceWheel.startOn sched rate.TimeSpan
        |> Observable.toList
        |> Observable.subscribe (fun lst -> lst |> should haveCount num)
        |> ignore

        sched.AdvanceBy(oneSec.TimeSpan)

module AlarmTests =

    [<Theory>]
    [<InlineData(60)>]
    [<InlineData(20)>]
    [<InlineData(10)>]
    [<InlineData(7)>]
    [<InlineData(5)>]
    let ``Alarm notifies at given rate per minute`` num =
        let sched = HistoricalScheduler(DateTimeOffset.Now)
        let freq = PerMinute (int64 num) |> toFrequency

        Alarm.scheduleOn sched None (Every (freq, After None))
        |> Observable.toList
        |> Observable.subscribe (fun lst -> lst |> should haveCount num)
        |> ignore
        
        sched.AdvanceBy(oneMin.TimeSpan)
        
    [<Theory>]
    [<InlineData(60)>]
    [<InlineData(20)>]
    [<InlineData(10)>]
    [<InlineData(5)>]
    [<InlineData(7)>]
    let ``Alarm notifies after delay at given rate per minute`` num =
        let sched = TestScheduler()
        let freq = PerMinute (int64 num) |> toFrequency
        let startAt = sched.Now + oneMin.TimeSpan - oneSec.TimeSpan |> Some

        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched None (Every (freq, After startAt))
        |> Observable.subscribe (actual.Add)
        |> ignore

        actual |> should be Empty
        sched.AdvanceTo((sched.Now + oneMin.TimeSpan).Ticks)
        actual |> should be Empty
        sched.AdvanceBy(oneMin.Value)
        actual |> should haveCount num
        
    [<Fact>]
    let ``Alarm is cancelled before notifying`` () =
        let sched = HistoricalScheduler(DateTimeOffset.Now)
        let actual = ResizeArray<PetitSonnerie>()
        let freq = PerMinute 5L |> toFrequency
        let startAt = sched.Now + oneMin.TimeSpan |> Some

        let alarm = Alarm.scheduleOn sched None (Every (freq, After startAt))
                    |> Observable.toList
                    |> Observable.subscribe actual.AddRange

        let advance = Seconds 30L
        sched.AdvanceBy(advance.TimeSpan)
        alarm.Dispose()
        actual |> should be Empty

    [<Theory>]
    [<InlineData(60)>]
    [<InlineData(20)>]
    [<InlineData(10)>]
    [<InlineData(7)>]
    [<InlineData(5)>]
    let ``Alarm notifies at given rate per hour`` num =
        let sched = HistoricalScheduler(DateTimeOffset.Now)
        let freq = PerHour (int64 num) |> toFrequency

        Alarm.scheduleOn sched None (Every (freq, After None))
        |> Observable.toList
        |> Observable.subscribe (fun lst -> lst |> should haveCount num)
        |> ignore

        let advance = Hours 1L
        sched.AdvanceBy(advance.TimeSpan)

    [<Theory>]
    [<InlineData(24)>]
    [<InlineData(8)>]
    [<InlineData(6)>]
    [<InlineData(5)>]
    [<InlineData(3)>]
    let ``Alarm notifies at given rate per day`` num =
        let sched = HistoricalScheduler(DateTimeOffset.Now)
        let freq = PerDay (int64 num) |> toFrequency

        Alarm.scheduleOn sched None (Every (freq, After None))
        |> Observable.toList
        |> Observable.subscribe (fun lst -> lst |> should haveCount num)
        |> ignore

        let advance = Days 1L
        sched.AdvanceBy(advance.TimeSpan)

    [<Fact>]
    let ``Alarm notifies every other week`` () =
        let sched = TestScheduler()
        let actual = ResizeArray<PetitSonnerie>()
        let freq = Weeks 2L |> toFrequency
        let recoilRate = (Days 1L).TimeSpan
        let oneWeek = Weeks 1L

        Alarm.scheduleOn sched (Some recoilRate) (Every (freq, After (Some sched.Now)))
        |> Observable.subscribe actual.Add
        |> ignore

        actual |> should be Empty
        sched.AdvanceBy(oneSec.Value)
        for i in [1..6] do
            sched.AdvanceBy(oneWeek.Value)
            actual |> should haveCount (i / 2)

    [<Fact>]
    let ``Alarm notifies every Friday at 0200 eastern`` () =
        let now = DateTimeOffset(2019, 1, 1, 3, 0, 0, 0, TimeSpan.Zero)
        let sched = HistoricalScheduler(now)
        let freq = Weeks 1L |> toFrequency
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()

        let zdt = ZonedDateTime.FromDateTimeOffset(now)
        let prev = DateAdjusters.Previous(IsoDayOfWeek.Friday)
        let next = DateAdjusters.Next(IsoDayOfWeek.Friday)
        let lastRun = zdt.Date.At(LocalTime(2,0))
                         .With(prev)
                         .WithOffset(DateTimeZoneProviders.Tzdb.GetSystemDefault().GetUtcOffset(zdt.ToInstant()))
                         .ToDateTimeOffset()
        let nextRun = OffsetDateTime.FromDateTimeOffset(lastRun).With(next).ToDateTimeOffset()

        Alarm.scheduleOn sched (Some recoilRate) (Every (freq, After (Some lastRun)))
        |> Observable.subscribe actual.Add
        |> ignore

        actual |> should be Empty
        sched.AdvanceBy(toTimeSpan freq)
        actual |> should not' (be Empty)
        actual |> should haveCount 1
        actual.[0].Time.ToDateTimeOffset() |> Alarm.normalize recoilRate
        |> should equal nextRun

    [<Fact>]
    let ``Alarm notifies weekly on Fridays at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 24, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let local = LocalTime(2, 0, 10)
        let dtz = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London")
        let interval = Weekly { alarm = { time = local; zone = dtz }; day = IsoDayOfWeek.Friday }
        
        let expected = DateTimeOffset(2018, 12, 28, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 1
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies daily at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 24, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 19, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let local = LocalTime(2, 0, 10)
        let dtz = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London")
        let interval = Daily { alarm = { time = local; zone = dtz }; kind = DailyKind.Everyday }
        
        let expected = DateTimeOffset(2018, 12, 25, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 7
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies on weekdays at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 24, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let local = LocalTime(2, 0, 10)
        let dtz = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London")
        let interval = Daily { alarm = { time = local; zone = dtz }; kind = DailyKind.Weekdays }
        
        let expected = DateTimeOffset(2018, 12, 25, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 4
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies on weekends at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 24, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let local = LocalTime(2, 0, 10)
        let dtz = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London")
        let interval = Daily { alarm = { time = local; zone = dtz }; kind = DailyKind.Weekends }
        
        let expected = DateTimeOffset(2018, 12, 29, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 2
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies fortnightly on Fridays at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 15, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 19, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let local = LocalTime(2, 0, 10)
        let dtz = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London")
        let interval = Fortnightly { alarm = { time = local; zone = dtz }; day = IsoDayOfWeek.Friday }
        
        let expected = DateTimeOffset(2018, 12, 28, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 1
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies monthly on the last day of month at 0200 London`` () =
        let now = DateTimeOffset(2018, 12, 15, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 30, 19, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let alarm = { time = LocalTime(2, 0, 10);
                      zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }
        let interval = Monthly { alarm = alarm; modifier = DayAdjustmentStrategy.Rigid; day = DayOfMonth.Last }
        
        let expected = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 1
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies semi-monthly, at the beginning and middle, at 0200 London`` () =
        let now = DateTimeOffset(2018, 11, 29, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 15, 19, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let alarm = { time = LocalTime(2, 0, 10);
                      zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }
        let interval = SemiMonthly { alarm = alarm; modifier = DayAdjustmentStrategy.WorkingDayBefore;
                                     firstDay = Day 1; secondDay = Day 15 }
        
        let expected1 = DateTimeOffset(2018, 11, 30, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 12, 14, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 2
        actual.[0].Time |> should equal expected1
        actual.[1].Time |> should equal expected2

    [<Fact>]
    let ``Alarm notifies annually on June 30 at 0200 London`` () =
        let now = DateTimeOffset(2018, 6, 29, 17, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 6, 30, 18, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let sched = HistoricalScheduler(now)
        
        let alarm = { time = LocalTime(2, 0, 10);
                      zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }
        let interval = Annually { alarm = alarm; modifier = DayAdjustmentStrategy.Rigid;
                                  date = AnnualDate(6, 30); adjustment = DateAdjustmentStrategy.Specific }
        
        let expected = DateTimeOffset(2018, 6, 30, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 1
        actual.[0].Time |> should equal expected

    [<Fact>]
    let ``Alarm notifies semi-annually, at end of June and December, at 0200 London`` () =
        let now = DateTimeOffset(2018, 6, 28, 17, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 31, 19, 0, 10, 0, TimeSpan.FromHours(-8.0))
        let sched = HistoricalScheduler(now)
        
        let alarm = { time = LocalTime(2, 0, 10);
                      zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }
        let interval = SemiAnnually { alarm = alarm; modifier = DayAdjustmentStrategy.WorkingDayBefore;
                                      date = AnnualDate(6, 30); adjustment = DateAdjustmentStrategy.Last }
        
        let expected1 = DateTimeOffset(2018, 6, 29, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 2
        actual.[0].Time |> should equal expected1
        actual.[1].Time |> should equal expected2

    [<Fact>]
    let ``Alarm notifies quarterly at 0200 London`` () =
        let now = DateTimeOffset(2018, 3, 29, 17, 0, 10, 0, TimeSpan.FromHours(-7.0))
        let later = DateTimeOffset(2018, 12, 31, 19, 0, 10, 0, TimeSpan.FromHours(-8.0))
        let sched = HistoricalScheduler(now)
        
        let alarm = { time = LocalTime(2, 0, 10);
                      zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }
        let interval = Quarterly { alarm = alarm; modifier = DayAdjustmentStrategy.WorkingDayBefore;
                                   date = AnnualDate(3, 31); adjustment = DateAdjustmentStrategy.Last }
        
        let expected1 = DateTimeOffset(2018, 3, 30, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 6, 29, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected3 = DateTimeOffset(2018, 9, 28, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected4 = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn sched (Some recoilRate) (Repeat (interval, Starting None))
        |> Observable.subscribe actual.Add
        |> ignore
        
        actual |> should be Empty
        sched.AdvanceTo(later)
        actual |> should not' (be Empty)
        actual |> should haveCount 4
        actual.[0].Time |> should equal expected1
        actual.[1].Time |> should equal expected2
        actual.[2].Time |> should equal expected3
        actual.[3].Time |> should equal expected4
