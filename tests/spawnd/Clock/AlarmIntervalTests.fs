module AlarmIntervalTests

open NodaTime
open Xunit
open FsUnit
open System
open Spawn.Clock.Time.Intervals
open Spawn.Clock.Utilities

let toIsoDayOfWeek = function 
    | DayOfWeek.Sunday    -> IsoDayOfWeek.Sunday
    | DayOfWeek.Monday    -> IsoDayOfWeek.Monday
    | DayOfWeek.Tuesday   -> IsoDayOfWeek.Tuesday
    | DayOfWeek.Wednesday -> IsoDayOfWeek.Wednesday
    | DayOfWeek.Thursday  -> IsoDayOfWeek.Thursday
    | DayOfWeek.Friday    -> IsoDayOfWeek.Friday
    | DayOfWeek.Saturday  -> IsoDayOfWeek.Saturday
    | x -> failwithf "[%A] is not a DayOfWeek" x

let toDayAdjustmentStrategy = function
    | 0 -> DayAdjustmentStrategy.Rigid
    | 1 -> DayAdjustmentStrategy.WorkingDayBefore
    | 2 -> DayAdjustmentStrategy.WorkingDayClosest
    | x -> failwithf "[%A] is not a DayOfMonth" x

module WeekdayTests =

    [<Theory>]
    [<InlineData("2018-10-14T09:00:00-0700", 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:00-0700", 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-29T02:00:10+0000")>]
    [<InlineData("2018-10-29T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-30T02:00:10+0000")>]
    [<InlineData("2018-10-30T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-10-31T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-01T02:00:10+0000")>]
    [<InlineData("2018-11-01T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-02T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-05T02:00:10+0000")>]
    [<InlineData("2018-11-03T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-05T02:00:10+0000")>]
    [<InlineData("2018-11-04T02:00:00-0700", 2, 0, @"Europe/London", "2018-11-05T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-05T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0700", 2, 0, @"Europe/London", "2019-03-11T02:00:10+0000")>]
    [<InlineData("2019-03-30T19:00:00-0700", 2, 0, @"Europe/London", "2019-04-01T02:00:10+0100")>]
    let ``Return UK next value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Daily { alarm = atTime; kind = DailyKind.Weekdays }

        let actual = given |> next interval

        actual |> should equal expected
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (greaterThanOrEqualTo IsoDayOfWeek.Monday)
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (lessThanOrEqualTo IsoDayOfWeek.Friday)

    [<Theory>]
    [<InlineData("2018-10-14T09:00:00-0700", 2, 0, @"Europe/London", "2018-10-12T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-10-29T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-29T02:00:10+0000")>]
    [<InlineData("2018-10-30T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-30T02:00:10+0000")>]
    [<InlineData("2018-10-31T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-11-01T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-01T02:00:10+0000")>]
    [<InlineData("2018-11-02T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-03T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-04T02:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0800", 2, 0, @"Europe/London", "2019-03-08T02:00:10+0000")>]
    [<InlineData("2019-03-30T19:00:00-0700", 2, 0, @"Europe/London", "2019-03-29T02:00:10+0000")>]
    [<InlineData("2019-04-07T12:00:00-0700", 2, 0, @"Europe/London", "2019-04-05T02:00:10+0100")>]
    let ``Return UK prior value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10);
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Daily { alarm = atTime; kind = DailyKind.Weekdays }

        let actual = given |> prev interval

        actual |> should equal expected
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (greaterThanOrEqualTo IsoDayOfWeek.Monday)
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (lessThanOrEqualTo IsoDayOfWeek.Friday)
  
module WeekendTests =

    [<Theory>]
    [<InlineData("2018-10-14T09:00:00-0700", 2, 0, @"Europe/London", "2018-10-20T02:00:10+0100")>]
    [<InlineData("2018-10-25T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-27T02:00:10+0100")>]
    [<InlineData("2018-10-26T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-10-27T19:00:11-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-10-28T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-10-29T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-11-01T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-11-02T12:00:00-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-11-03T02:00:00-0700", 2, 0, @"Europe/London", "2018-11-04T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-10T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0700", 2, 0, @"Europe/London", "2019-03-16T02:00:10+0000")>]
    [<InlineData("2019-03-29T19:00:00-0700", 2, 0, @"Europe/London", "2019-03-30T02:00:10+0000")>]
    [<InlineData("2019-03-29T19:00:11-0700", 2, 0, @"Europe/London", "2019-03-31T02:00:10+0100")>]
    let `` Return UK next value given California Value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10);
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Daily { alarm = atTime; kind = DailyKind.Weekends }

        let actual = given |> next interval

        actual |> should equal expected
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (greaterThanOrEqualTo IsoDayOfWeek.Saturday)
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (lessThanOrEqualTo IsoDayOfWeek.Sunday)

    [<Theory>]
    [<InlineData("2018-10-14T09:00:00-0700", 2, 0, @"Europe/London", "2018-10-14T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-27T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:11-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-10-28T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-10-29T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-11-01T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-11-02T12:00:00-0700", 2, 0, @"Europe/London", "2018-10-28T02:00:10+0000")>]
    [<InlineData("2018-11-03T02:00:00-0700", 2, 0, @"Europe/London", "2018-11-03T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-04T02:00:10+0000")>]
    [<InlineData("2019-03-10T03:00:00-0700", 2, 0, @"Europe/London", "2019-03-10T02:00:10+0000")>]
    [<InlineData("2019-03-30T18:00:00-0700", 2, 0, @"Europe/London", "2019-03-30T02:00:10+0000")>]
    [<InlineData("2019-03-30T18:00:11-0700", 2, 0, @"Europe/London", "2019-03-31T02:00:10+0100")>]
    let ``Return UK prior value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Daily { alarm = atTime; kind = DailyKind.Weekends }

        let actual = given |> prev interval

        actual |> should equal expected
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (greaterThanOrEqualTo IsoDayOfWeek.Saturday)
        actual.ToDateTimeOffset().DayOfWeek |> toIsoDayOfWeek 
                                            |> should be (lessThanOrEqualTo IsoDayOfWeek.Sunday)

module WeeklyTests =

    [<Theory>]
    [<InlineData("2018-10-08T18:00:00-0700", 2, 0, @"Europe/London", "2018-10-12T02:00:10+0100")>]
    [<InlineData("2018-10-27T17:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-10-27T18:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-04T01:00:00-0700", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2018-11-04T02:00:00-0800", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2019-03-10T01:00:00-0800", 2, 0, @"Europe/London", "2019-03-15T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0700", 2, 0, @"Europe/London", "2019-03-15T02:00:10+0000")>]
    [<InlineData("2019-03-10T03:00:00-0700", 2, 0, @"Europe/London", "2019-03-15T02:00:10+0000")>]
    [<InlineData("2019-03-30T17:00:00-0700", 2, 0, @"Europe/London", "2019-04-05T02:00:10+0100")>]
    [<InlineData("2019-03-30T18:00:00-0700", 2, 0, @"Europe/London", "2019-04-05T02:00:10+0100")>]
    [<InlineData("2019-03-30T19:00:00-0700", 2, 0, @"Europe/London", "2019-04-05T02:00:10+0100")>]
    let ``Return UK next value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Weekly { alarm = atTime; day = IsoDayOfWeek.Friday }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-10-04T18:00:11-0700", 2, 0, @"Europe/London", "2018-10-05T02:00:10+0100")>]
    [<InlineData("2018-10-27T17:00:00-0700", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-10-27T18:00:00-0700", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-11-04T01:00:00-0700", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-04T02:00:00-0800", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-02T02:00:10+0000")>]
    [<InlineData("2019-03-10T01:00:00-0700", 2, 0, @"Europe/London", "2019-03-08T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0800", 2, 0, @"Europe/London", "2019-03-08T02:00:10+0000")>]
    [<InlineData("2019-03-10T03:00:00-0800", 2, 0, @"Europe/London", "2019-03-08T02:00:10+0000")>]
    [<InlineData("2019-03-30T17:00:00-0800", 2, 0, @"Europe/London", "2019-03-29T02:00:10+0000")>]
    [<InlineData("2019-03-30T18:00:00-0800", 2, 0, @"Europe/London", "2019-03-29T02:00:10+0000")>]
    [<InlineData("2019-03-30T19:00:00-0800", 2, 0, @"Europe/London", "2019-03-29T02:00:10+0000")>]
    let ``Return UK prior value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10);
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Weekly { alarm = atTime; day = IsoDayOfWeek.Friday }

        let actual = given |> prev interval

        actual |> should equal expected

module FortnightlyAndBiWeeklyTests =
    
    [<Theory>]
    [<InlineData("2018-10-11T18:00:09-0700", 2, 0, @"Europe/London", "2018-10-19T02:00:10+0100")>]
    [<InlineData("2018-10-27T17:00:00-0700", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2018-10-27T18:00:00-0700", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-11-09T02:00:10+0000")>]
    [<InlineData("2018-11-04T01:00:00-0700", 2, 0, @"Europe/London", "2018-11-16T02:00:10+0000")>]
    [<InlineData("2018-11-04T02:00:00-0800", 2, 0, @"Europe/London", "2018-11-16T02:00:10+0000")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-11-16T02:00:10+0000")>]
    [<InlineData("2019-03-10T01:00:00-0800", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0700", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    [<InlineData("2019-03-10T03:00:00-0700", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    [<InlineData("2019-03-30T17:00:00-0700", 2, 0, @"Europe/London", "2019-04-12T02:00:10+0100")>]
    [<InlineData("2019-03-30T18:00:00-0700", 2, 0, @"Europe/London", "2019-04-12T02:00:10+0100")>]
    [<InlineData("2019-03-30T19:00:00-0700", 2, 0, @"Europe/London", "2019-04-12T02:00:10+0100")>]
    let ``Return UK next value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Fortnightly { alarm = atTime; day = IsoDayOfWeek.Friday }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-10-11T18:00:11-0700", 2, 0, @"Europe/London", "2018-10-05T02:00:10+0100")>]
    [<InlineData("2018-10-27T17:00:00-0700", 2, 0, @"Europe/London", "2018-10-19T02:00:10+0100")>]
    [<InlineData("2018-10-27T18:00:00-0700", 2, 0, @"Europe/London", "2018-10-19T02:00:10+0100")>]
    [<InlineData("2018-10-27T19:00:00-0700", 2, 0, @"Europe/London", "2018-10-19T02:00:10+0100")>]
    [<InlineData("2018-11-04T01:00:00-0700", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-11-04T02:00:00-0800", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2018-11-04T03:00:00-0800", 2, 0, @"Europe/London", "2018-10-26T02:00:10+0100")>]
    [<InlineData("2019-03-10T01:00:00-0700", 2, 0, @"Europe/London", "2019-03-01T02:00:10+0000")>]
    [<InlineData("2019-03-10T02:00:00-0800", 2, 0, @"Europe/London", "2019-03-01T02:00:10+0000")>]
    [<InlineData("2019-03-10T03:00:00-0800", 2, 0, @"Europe/London", "2019-03-01T02:00:10+0000")>]
    [<InlineData("2019-03-30T17:00:00-0800", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    [<InlineData("2019-03-30T18:00:00-0800", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    [<InlineData("2019-03-30T19:00:00-0800", 2, 0, @"Europe/London", "2019-03-22T02:00:10+0000")>]
    let ``Return UK prior value given California value`` (givenCA, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Fortnightly { alarm = atTime; day = IsoDayOfWeek.Friday }

        let actual = given |> prev interval

        actual |> should equal expected

module MonthlyTests =

    [<Theory>]
    [<InlineData("2018-10-11T18:00:09-0700", 15, 0, 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:10-0700", 15, 0, 2, 0, @"Europe/London", "2018-11-15T02:00:10+0000")>]
    [<InlineData("2018-10-31T19:00:09-0700",  1, 0, 2, 0, @"Europe/London", "2018-11-01T02:00:10+0000")>]
    [<InlineData("2018-10-31T19:00:11-0700",  1, 0, 2, 0, @"Europe/London", "2018-12-01T02:00:10+0000")>]
    [<InlineData("2018-10-30T19:00:09-0700", 32, 0, 2, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-10-30T19:00:10-0700", 32, 0, 2, 0, @"Europe/London", "2018-11-30T02:00:10+0000")>]
    [<InlineData("2018-10-14T18:00:09-0700", 15, 1, 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-11-14T18:00:09-0800", 15, 1, 2, 0, @"Europe/London", "2018-11-15T02:00:10+0000")>]
    [<InlineData("2018-12-13T18:00:09-0800", 15, 1, 2, 0, @"Europe/London", "2018-12-14T02:00:10+0000")>]
    [<InlineData("2018-12-14T18:00:09-0800", 15, 1, 2, 0, @"Europe/London", "2019-01-15T02:00:10+0000")>]
    [<InlineData("2018-11-29T18:00:09-0800",  1, 1, 2, 0, @"Europe/London", "2018-11-30T02:00:10+0000")>]
    [<InlineData("2018-11-29T18:00:10-0800",  1, 1, 2, 0, @"Europe/London", "2019-01-01T02:00:10+0000")>]
    let ``Return UK next value given California value`` 
        (givenCA, dayOfMonth, strategy, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let dom = match dayOfMonth with
                  | x when x = 32 -> Last
                  | x when x > 0 && x < 32 -> Day x
                  | x -> failwithf "%d: Illegal day of month value" x
        let adjStrategy = toDayAdjustmentStrategy strategy
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Monthly { alarm = atTime; modifier = adjStrategy; day = dom }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-10-14T18:00:09-0700", 15, 0, 2, 0, @"Europe/London", "2018-09-15T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:11-0700", 15, 0, 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-31T19:00:09-0700",  1, 0, 2, 0, @"Europe/London", "2018-10-01T02:00:10+0100")>]
    [<InlineData("2018-10-31T19:00:11-0700",  1, 0, 2, 0, @"Europe/London", "2018-11-01T02:00:10+0000")>]
    [<InlineData("2018-10-30T19:00:10-0700", 32, 0, 2, 0, @"Europe/London", "2018-09-30T02:00:10+0100")>]
    [<InlineData("2018-10-30T19:00:11-0700", 32, 0, 2, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-10-14T18:00:09-0700", 15, 1, 2, 0, @"Europe/London", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-11-14T18:00:09-0800", 15, 1, 2, 0, @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-12-13T18:00:10-0800", 15, 1, 2, 0, @"Europe/London", "2018-11-15T02:00:10+0000")>]
    [<InlineData("2018-12-13T18:00:11-0800", 15, 1, 2, 0, @"Europe/London", "2018-12-14T02:00:10+0000")>]
    let ``Return UK previous value given California value`` 
        (givenCA, dayOfMonth, strategy, hour, min, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let dom = match dayOfMonth with
                  | x when x = 32 -> Last
                  | x when x > 0 && x < 32 -> Day x
                  | x -> failwithf "%d: Illegal day of month value" x
        let adjStrategy = toDayAdjustmentStrategy strategy
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Monthly { alarm = atTime; modifier = adjStrategy; day = dom }

        let actual = given |> prev interval

        actual |> should equal expected

module AnnuallyTests =

    [<Theory>]
    [<InlineData("2018-06-28T18:00:09-0700",  6, 30, 2, 0, 1, @"Europe/London", "2018-06-29T02:00:10+0100")>]
    [<InlineData("2018-06-28T18:00:10-0700",  6, 30, 2, 0, 1, @"Europe/London", "2019-06-28T02:00:10+0100")>]
    [<InlineData("2018-08-30T18:00:09-0700",  8, 31, 2, 0, 1, @"Europe/London", "2018-08-31T02:00:10+0100")>]
    [<InlineData("2018-08-30T18:00:11-0700",  8, 31, 2, 0, 1, @"Europe/London", "2019-08-30T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:09-0700",  9, 30, 2, 0, 1, @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:11-0700",  9, 30, 2, 0, 1, @"Europe/London", "2019-09-30T02:00:10+0100")>]
    [<InlineData("2018-10-30T19:00:09-0700", 10, 31, 2, 0, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-10-30T19:00:11-0700", 10, 31, 2, 0, 0, @"Europe/London", "2019-10-31T02:00:10+0000")>]
    let ``Return UK next value given California value`` 
        (givenCA, month, day, hour, min, strategy, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let adjStrategy = toDayAdjustmentStrategy strategy
        let date = AnnualDate(month, day)
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Annually { alarm = atTime; modifier = adjStrategy;
                                  date = date; adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-06-28T18:00:10-0700",  6, 30, 2, 0, 1, @"Europe/London", "2017-06-30T02:00:10+0100")>]
    [<InlineData("2018-06-28T18:00:11-0700",  6, 30, 2, 0, 1, @"Europe/London", "2018-06-29T02:00:10+0100")>]
    [<InlineData("2018-08-30T18:00:09-0700",  8, 31, 2, 0, 1, @"Europe/London", "2017-08-31T02:00:10+0100")>]
    [<InlineData("2018-08-30T18:00:11-0700",  8, 31, 2, 0, 1, @"Europe/London", "2018-08-31T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:09-0700",  9, 30, 2, 0, 1, @"Europe/London", "2017-09-29T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:11-0700",  9, 30, 2, 0, 1, @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-10-30T19:00:09-0700", 10, 31, 2, 0, 0, @"Europe/London", "2017-10-31T02:00:10+0000")>]
    [<InlineData("2018-10-30T19:00:11-0700", 10, 31, 2, 0, 0, @"Europe/London", "2018-10-31T02:00:10+0000")>]
    let ``Return UK previous value given California value`` 
        (givenCA, month, day, hour, min, strategy, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let adjStrategy = toDayAdjustmentStrategy strategy
        let date = AnnualDate(month, day)
        let atTime = { time = LocalTime(hour, min, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }

        let interval = Annually { alarm = atTime; modifier = adjStrategy;
                                  date = date; adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> prev interval

        actual |> should equal expected

module SemiMonthlyTests =

    [<Theory>]
    [<InlineData("2018-10-11T18:00:09-0700", @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:09-0700", @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:10-0700", @"Europe/London", "2018-10-31T02:00:10+0000")>]
    [<InlineData("2018-09-27T18:00:09-0700", @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:11-0700", @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:09-0700", @"Europe/London", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:10-0700", @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<Trait("Kind", "SemiMonthly")>]
    [<Trait("Direction", "Next")>]
    let ``Return UK next value given California value`` (givenCA, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }
        
        let interval = SemiMonthly { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                     firstDay = Day 15; secondDay = Last }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-10-11T18:00:09-0700", @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:10-0700", @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-10-14T18:00:11-0700", @"Europe/London", "2018-10-15T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:09-0700", @"Europe/London", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-09-27T18:00:11-0700", @"Europe/London", "2018-09-28T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:10-0700", @"Europe/London", "2018-08-31T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:11-0700", @"Europe/London", "2018-09-14T02:00:10+0100")>]
    [<Trait("Kind", "SemiMonthly")>]
    [<Trait("Direction", "Prev")>]
    let ``Return UK previous value given California value`` (givenCA, timeZone, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(timeZone) }
        
        let interval = SemiMonthly { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                     firstDay = Last; secondDay = Day 15 }

        let actual = given |> prev interval

        actual |> should equal expected

module SemiAnnuallyTests =

    [<Theory>]
    [<InlineData("2018-03-14T19:00:09-0700", "2018-03-15T02:00:10+0000")>]
    [<InlineData("2018-03-14T19:00:11-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:09-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:10-0700", "2019-03-15T02:00:10+0000")>]
    [<InlineData("2019-03-14T19:00:11-0700", "2019-09-13T02:00:10+0100")>]
    let ``Return UK next value given California value`` (givenCA, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }

        let interval = SemiAnnually { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                      date = AnnualDate(9, 15); adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-03-14T19:00:11-0700", "2018-03-15T02:00:10+0000")>]
    [<InlineData("2018-09-13T18:00:10-0700", "2018-03-15T02:00:10+0000")>]
    [<InlineData("2018-09-13T19:00:11-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2019-03-14T18:00:09-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2019-09-12T18:00:10-0700", "2019-03-15T02:00:10+0000")>]
    let ``Return UK previous value given California value`` (givenCA, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }

        let interval = SemiAnnually { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                      date = AnnualDate(3, 15); adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> prev interval

        actual |> should equal expected

module QuarterlyTests =

    [<Theory>]
    [<InlineData("2018-03-14T19:00:09-0700", "2018-03-15T02:00:10+0000")>]
    [<InlineData("2018-03-14T19:00:11-0700", "2018-06-15T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:09-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2018-09-13T18:00:10-0700", "2018-12-14T02:00:10+0000")>]
    [<InlineData("2018-12-14T19:00:11-0800", "2019-03-15T02:00:10+0000")>]
    let ``Return UK next value given California value`` (givenCA, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }

        let interval = Quarterly { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                   date = AnnualDate(3, 15); adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> next interval

        actual |> should equal expected
    
    [<Theory>]
    [<InlineData("2018-03-14T19:00:11-0700", "2018-03-15T02:00:10+0000")>]
    [<InlineData("2018-09-13T18:00:10-0700", "2018-06-15T02:00:10+0100")>]
    [<InlineData("2018-09-13T19:00:11-0700", "2018-09-14T02:00:10+0100")>]
    [<InlineData("2019-03-14T18:00:09-0700", "2018-12-14T02:00:10+0000")>]
    [<InlineData("2019-09-12T18:00:10-0700", "2019-06-14T02:00:10+0100")>]
    let ``Return UK previous value given California value`` (givenCA, expectedUK) =
        let given = DateTimeOffset.Parse(givenCA) |> toInstant
        let expected = DateTimeOffset.Parse(expectedUK) |> toInstant
        let atTime = { time = LocalTime(2, 0, 10); 
                       zone = DateTimeZoneProviders.Tzdb.GetZoneOrNull(@"Europe/London") }

        let interval = Quarterly { alarm = atTime; modifier = DayAdjustmentStrategy.WorkingDayBefore; 
                                   date = AnnualDate(9, 15); adjustment = DateAdjustmentStrategy.Specific }

        let actual = given |> prev interval

        actual |> should equal expected
