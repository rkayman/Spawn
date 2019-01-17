module ClockTests

open NodaTime
open Xunit
open FsUnit
open FSharp.Control.Reactive
open FSharp.Control.Reactive.Testing
open Microsoft.Reactive.Testing
open System.Reactive.Concurrency
open System
open Spawn.Clock
open Spawn.Clock.Alarm
open Spawn.Clock.Repeater
open Spawn.Clock.Time
open Spawn.Clock.Time.Intervals


let private oneSec = Sec 1L
let private oneMin = Min 1L


module BalanceWheelTests =

    [<Fact>]
    let ``BalanceWheel coils at fixed frequency`` () =
        let freq = Ticks 100L
        let subscriptionOffset = Ticks 200L

        let timeOffset x = subscriptionOffset.Value + ((x + 1L) * freq.Value)
        let makeNotification x = TestNotification.onNext (timeOffset x) x

        let expected = [0L..6L] |> List.map makeNotification

        let sched = TestScheduler()
        let hb = BalanceWheel.startOn freq.TimeSpan sched
                 |> Observable.map (fun x -> x.Count)

        let actual = TestSchedule.startObservable sched hb 
                     |> TestObserver.all
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
        let rate = PerSec (int64 num)

        BalanceWheel.startOn rate.TimeSpan sched
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
        let rate = PerMin (int64 num)

        Alarm.scheduleOn (Every (Rate rate, None)) sched None
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
        let rate = PerMin (int64 num)
        let startAt = sched.Now + oneMin.TimeSpan - oneSec.TimeSpan

        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Every (Rate rate, Some startAt)) sched None
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
        let rate = PerMin 5L
        let startAt = sched.Now + oneMin.TimeSpan

        let alarm = Alarm.scheduleOn (Every (Rate rate, Some startAt)) sched None
                    |> Observable.toList
                    |> Observable.subscribe actual.AddRange

        let advance = Sec 30L
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
        let rate = PerHour (int64 num)

        Alarm.scheduleOn (Every (Rate rate, None)) sched None
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
        let rate = PerDay (int64 num)

        Alarm.scheduleOn (Every (Rate rate, None)) sched None
        |> Observable.toList
        |> Observable.subscribe (fun lst -> lst |> should haveCount num)
        |> ignore

        let advance = Days 1L
        sched.AdvanceBy(advance.TimeSpan)

    [<Fact>]
    let ``Alarm notifies every other week`` () =
        let sched = TestScheduler()
        let actual = ResizeArray<PetitSonnerie>()
        let freq = Weeks 2L
        let recoilRate = (Days 1L).TimeSpan
        let week = (Weeks 1L).Value

        Alarm.scheduleOn (Every (Frequency freq, None)) sched (Some recoilRate)
        |> Observable.subscribe actual.Add
        |> ignore

        actual |> should be Empty
        sched.AdvanceBy(oneSec.Value)
        for i in [1..6] do
            sched.AdvanceBy(week)
            actual |> should haveCount (i / 2)

    [<Fact>]
    let ``Alarm notifies every Friday at 0200 eastern`` () =
        let now = DateTimeOffset(2019, 1, 1, 3, 0, 0, 0, TimeSpan.Zero)
        let sched = HistoricalScheduler(now)
        let freq = Weeks 1L
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

        Alarm.scheduleOn (Every (Frequency freq, Some lastRun)) sched (Some recoilRate)
        |> Observable.subscribe actual.Add
        |> ignore

        actual |> should be Empty
        sched.AdvanceBy(freq.TimeSpan)
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
        let pattern = Weekly { alarm = { time = local; zone = dtz }; day = IsoDayOfWeek.Friday }
        
        let expected = DateTimeOffset(2018, 12, 28, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Daily { alarm = { time = local; zone = dtz }; kind = Everyday }
        
        let expected = DateTimeOffset(2018, 12, 25, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Daily { alarm = { time = local; zone = dtz }; kind = Weekdays }
        
        let expected = DateTimeOffset(2018, 12, 25, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Daily { alarm = { time = local; zone = dtz }; kind = Weekends }
        
        let expected = DateTimeOffset(2018, 12, 29, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Fortnightly { alarm = { time = local; zone = dtz }; day = IsoDayOfWeek.Friday }
        
        let expected = DateTimeOffset(2018, 12, 28, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Monthly { alarm = alarm; modifier = Rigid; day = DayOfMonth.Last }
        
        let expected = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = SemiMonthly { alarm = alarm; modifier = WorkingDayBefore;
                                    firstDay = DayOfMonth.First; secondDay = DayOfMonth.Fifteenth }
        
        let expected1 = DateTimeOffset(2018, 11, 30, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 12, 14, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Annually { alarm = alarm; modifier = Rigid;
                                 date = AnnualDate(6, 30); adjustment = Specific }
        
        let expected = DateTimeOffset(2018, 6, 30, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = SemiAnnually { alarm = alarm; modifier = WorkingDayBefore;
                                     date = AnnualDate(6, 30); adjustment = Last }
        
        let expected1 = DateTimeOffset(2018, 6, 29, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
        let pattern = Quarterly { alarm = alarm; modifier = WorkingDayBefore;
                                  date = AnnualDate(3, 31); adjustment = Last }
        
        let expected1 = DateTimeOffset(2018, 3, 30, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected2 = DateTimeOffset(2018, 6, 29, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected3 = DateTimeOffset(2018, 9, 28, 2, 0, 10, 0, TimeSpan.FromHours(1.0)) |> Instant.FromDateTimeOffset
        let expected4 = DateTimeOffset(2018, 12, 31, 2, 0, 10, 0, TimeSpan.Zero) |> Instant.FromDateTimeOffset
        
        let recoilRate = (Hours 1L).TimeSpan
        let actual = ResizeArray<PetitSonnerie>()
        Alarm.scheduleOn (Schedule (pattern, None)) sched (Some recoilRate)
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
