namespace Spawn

open NodaTime
open System

module TimeUtilities =

    type Frequency =
        | Ticks of int64
        | Ms of int64
        | Sec of int64
        | Min of int64
        | Hours of int64
        | Days of int64
        | Weeks of int64

    type Rate =
        | PerSec of int64
        | PerMin of int64
        | PerHour of int64
        | PerDay of int64
        
    let private frequency = function
        | Ticks t -> t
        | Ms ms -> ms * NodaConstants.TicksPerMillisecond
        | Sec s -> s * NodaConstants.TicksPerSecond
        | Min m -> m * NodaConstants.TicksPerMinute
        | Hours h -> h * NodaConstants.TicksPerHour
        | Days d -> d * NodaConstants.TicksPerDay
        | Weeks w -> w * NodaConstants.TicksPerWeek

    type Frequency with
        member this.Value with get() = frequency this
        member this.TimeSpan with get() = this |> (frequency >> TimeSpan.FromTicks)
        
    let private computeRate ((rate, period, ticks): int64 * decimal * int64) = 
        let r, t = decimal rate, decimal ticks
        Math.Round(period / r, 3) * t |> int64

    let private rate = function
        | PerSec s -> (s, 1000m, NodaConstants.TicksPerMillisecond) |> computeRate
        | PerMin m -> (m, 60m, NodaConstants.TicksPerSecond) |> computeRate
        | PerHour h -> (h, 60m, NodaConstants.TicksPerMinute) |> computeRate
        | PerDay d -> (d, 24m, NodaConstants.TicksPerHour) |> computeRate
    
    type Rate with
        member this.Value with get() = rate this
        member this.TimeSpan with get() = this |> (rate >> TimeSpan.FromTicks)


module NodaUtilities =

    type TimeAdjusters = ToSecond | ToMinute | ToHour

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
