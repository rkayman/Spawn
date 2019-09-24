namespace Spawn

open FSharpPlus
open Fleece.FSharpData.Helpers
open Fleece.FSharpData.Operators
open Fleece.FSharpData
open NodaTime
open System

module Configuration =
    
    type Agenda =
        { alarms: Alarm[] }
        
    and Alarm =
        { domain : string
          name : string
          payload : Payload
          schedule : Schedule }
        
    and AlarmKey =
        { domain : string
          name : string }
                
    and Payload =
        { source : string
          protocol : string
          format : string
          feed : string
          recordType : string
          batchSize : int
          maxRetries : int }
        
    and Schedule =
        | Frequency    of Frequency
        | Daily        of DailyInterval
        | Weekly       of WeeklyInterval
        | Fortnightly  of WeeklyInterval
        | SemiMonthly  of TwiceMonthlyInterval
        | Monthly      of MonthlyInterval
        | Quarterly    of AnnualInterval
        | SemiAnnually of AnnualInterval
        | Annually     of AnnualInterval
        
    and Frequency = { size: int64; unit : UnitOfTime }
            
    and UnitOfTime =
        | Ticks        = 0
        | Milliseconds = 1
        | Seconds      = 2
        | Minutes      = 3
        | Hours        = 4
        | Days         = 5
        | Weeks        = 6
        | PerSecond    = 7
        | PerMinute    = 8
        | PerHour      = 9
        | PerDay       = 10
        
    and AlarmTime            = { time: LocalTime; zone: DateTimeZone }
    and DailyInterval        = { alarm: AlarmTime; kind: DailyKind }
    and WeeklyInterval       = { alarm: AlarmTime; day: IsoDayOfWeek }
    and MonthlyInterval      = { alarm: AlarmTime; modifier: DayAdjustmentStrategy; day: DayOfMonth }
    and TwiceMonthlyInterval = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                                 firstDay: DayOfMonth; secondDay: DayOfMonth }
    and AnnualInterval       = { alarm: AlarmTime; modifier: DayAdjustmentStrategy;
                                 date: AnnualDate; adjustment: DateAdjustmentStrategy }
    
    and DayOfMonth             = Day of int | Last
    and DailyKind              = Everyday = 0 | Weekdays = 1 | Weekends = 2
    and DayAdjustmentStrategy  = Rigid = 0 | WorkingDayBefore = 1 | WorkingDayClosest = 2
    and DateAdjustmentStrategy = Specific = 0 | Last = 1


    let inline private toString (x: 'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType) =
        Enum.GetName(typeof<'a>, x)
        
    let private asEnum<'a, 'b when 'a: (new: unit -> 'a)
                               and 'a: struct
                               and 'a :> ValueType
                               and 'a: enum<'b>> json =
        let failure = Decode.Fail.invalidValue json
                          (sprintf "Expected %s value but found: %A" (typeof<'a>.Name) json)

        match json with
        | JString x -> match Enum.TryParse<'a>(x) with
                       | true, value  -> Success value
                       | _            -> failure
        | _         -> failure
        
    let inline private ofEnum (x: 'a when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType) =
        x |> toString |> JString
        
    let private makeEnumCodec<'a, 'b when 'a: (new: unit -> 'a) and 'a: struct and 'a :> ValueType and 'a: enum<'b>> () =
        let decoder = asEnum<'a,_>
        let encoder (x: 'a) = ofEnum x
        decoder, encoder
        
    let private unitOfTimeCodec = makeEnumCodec<UnitOfTime,_> ()
    let private dailyKindCodec = makeEnumCodec<DailyKind,_> ()
    let private dayAdjustmentCodec = makeEnumCodec<DayAdjustmentStrategy,_> ()
    let private dateAdjustmentCodec = makeEnumCodec<DateAdjustmentStrategy,_> ()
    let private isoDayOfWeekCodec = makeEnumCodec<IsoDayOfWeek,_> ()
    
    let inline private localTimeToJson (x: LocalTime) =
        sprintf "%02d:%02d:%02d" x.Hour x.Minute x.Second |> JString
        
    let private (|LocalTime|InvalidTime|) (str: string) =
        try
            let els = str.Split(':') |> Array.map int
            if els.Length = 3 then LocalTime (NodaTime.LocalTime(els.[0], els.[1], els.[2]))
            else InvalidTime
        with :? System.FormatException -> InvalidTime
        
    let private jsonToLocalTime = function
        | JString x as json -> match x with
                               | LocalTime t -> Success t
                               | _ -> Decode.Fail.invalidValue json "Expected a LocalTime as string (hh:mm:ss)"
        | json -> Decode.Fail.strExpected json
        
    let private localTimeCodec = jsonToLocalTime, localTimeToJson
    
    let inline private dateTimeZoneToJson (x: DateTimeZone) = JString x.Id
    
    let private (|TimeZone|InvalidTimeZone|) (str: string) =
        match DateTimeZoneProviders.Tzdb.GetZoneOrNull(str) with
        | null -> InvalidTimeZone(sprintf "Time zone not found: %s" str)
        | zone -> TimeZone zone
    
    let private jsonToDateTimeZone = function
        | JString x as json -> match x with
                               | TimeZone z -> Success z
                               | InvalidTimeZone str -> Decode.Fail.invalidValue json str
        | json -> Decode.Fail.strExpected json
        
    let private dateTimeZoneCodec = jsonToDateTimeZone, dateTimeZoneToJson
    
    let private dayOfMonthToJson (x: DayOfMonth) =
        match x with
        | Day num -> string num |> JString
        | Last    -> JString "last"
        
    let private jsonToDayOfMonth = function
        | JString "last" -> Success Last
        | JString x as json ->
            let isValid n = n > 0 && n < 29
            match Int32.TryParse(x) with
            | true, d when isValid d -> Success (Day d)
            | _ -> Decode.Fail.invalidValue json "Expected a DayOfMonth value"
        | json -> Decode.Fail.invalidValue json "Expected a DayOfMonth value"
        
    let private dayOfMonthCodec = jsonToDayOfMonth, dayOfMonthToJson
    
    let private months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
    
    let private annualDateToJson (x: AnnualDate) =
        let month = months.[x.Month - 1]
        sprintf "%s-%02d" month x.Day |> JString
        
    let private jsonToAnnualDate json =
        let err = Decode.Fail.invalidValue json "Expected an AnnualDate as 'MMM-dd' (e.g. Jan-05)"
        match json with
        | JString x when x.Contains("-") ->
            let xs = x.Split([|'-'|], StringSplitOptions.None)
            let m,d = xs.[0], xs.[1] |> int
            let eq (x: string) = (m.ToLowerInvariant(), x.ToLowerInvariant()) |> String.Equals
            months |> Array.tryFindIndex eq
                   |> Option.map (fun mon -> Success (AnnualDate(mon + 1, d)))
                   |> Option.defaultValue err                          
        | _ -> err
        
    let annualDateCodec = jsonToAnnualDate, annualDateToJson
    
    type Payload with
        static member JsonObjCodec =
            fun s p fmt f r b m -> { source = s; protocol = p; format = fmt; feed = f;
                                     recordType = r; batchSize = b; maxRetries = m }
            <!> jreq "source"     (fun x -> Some x.source)
            <*> jreq "protocol"   (fun x -> Some x.protocol)
            <*> jreq "format"     (fun x -> Some x.format)
            <*> jreq "feed"       (fun x -> Some x.feed)
            <*> jreq "recordType" (fun x -> Some x.recordType)
            <*> jreq "batchSize"  (fun x -> Some x.batchSize)
            <*> jreq "maxRetries" (fun x -> Some x.maxRetries)
    
    let private tag prop codec =
        Codec.ofConcrete codec
        |> Codec.compose
                (
                    (fun o -> match Seq.toList o with
                              | [KeyValue(p, JObject a)] when p = prop -> Ok a
                              | _ -> Decode.Fail.propertyNotFound prop o), 
                    (fun x -> if Seq.isEmpty x then zero else Dict.toIReadOnlyDictionary (dict [prop, JObject x]))
                )
        |> Codec.toConcrete

    let private annuallyCodec =
        fun t z m d a -> Annually { alarm = { time = t; zone =z }; modifier = m; date = d; adjustment = a }
        <!> jreqWith localTimeCodec      "time"       (function Annually { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec   "zone"       (function Annually { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dayAdjustmentCodec  "modifier"   (function Annually { modifier = x } -> Some x | _ -> None)
        <*> jreqWith annualDateCodec     "date"       (function Annually { date = x } -> Some x | _ -> None)
        <*> jreqWith dateAdjustmentCodec "adjustment" (function Annually { adjustment = x } -> Some x | _ -> None)

    let private semiAnnuallyCodec =
        fun t z m d a -> SemiAnnually { alarm = { time = t; zone =z }; modifier = m; date = d; adjustment = a }
        <!> jreqWith localTimeCodec      "time"       (function SemiAnnually { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec   "zone"       (function SemiAnnually { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dayAdjustmentCodec  "modifier"   (function SemiAnnually { modifier = x } -> Some x | _ -> None)
        <*> jreqWith annualDateCodec     "date"       (function SemiAnnually { date = x } -> Some x | _ -> None)
        <*> jreqWith dateAdjustmentCodec "adjustment" (function SemiAnnually { adjustment = x } -> Some x | _ -> None)

    let private quarterlyCodec =
        fun t z m d a -> Quarterly { alarm = { time = t; zone =z }; modifier = m; date = d; adjustment = a }
        <!> jreqWith localTimeCodec      "time"       (function Quarterly { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec   "zone"       (function Quarterly { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dayAdjustmentCodec  "modifier"   (function Quarterly { modifier = x } -> Some x | _ -> None)
        <*> jreqWith annualDateCodec     "date"       (function Quarterly { date = x } -> Some x | _ -> None)
        <*> jreqWith dateAdjustmentCodec "adjustment" (function Quarterly { adjustment = x } -> Some x | _ -> None)
            
    let private monthlyCodec =
        fun t z m d -> Monthly { alarm = { time = t; zone = z }; modifier = m; day = d }
        <!> jreqWith localTimeCodec     "time"     (function Monthly { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec  "zone"     (function Monthly { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dayAdjustmentCodec "modifier" (function Monthly { modifier = x } -> Some x | _ -> None)
        <*> jreqWith dayOfMonthCodec    "day"      (function Monthly { day = x } -> Some x | _ -> None)

    let private semiMonthlyCodec =
        fun t z m f s -> SemiMonthly { alarm = { time = t; zone = z }; modifier = m; firstDay = f; secondDay = s }
        <!> jreqWith localTimeCodec     "time"      (function SemiMonthly { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec  "zone"      (function SemiMonthly { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dayAdjustmentCodec "modifier"  (function SemiMonthly { modifier = x } -> Some x | _ -> None)
        <*> jreqWith dayOfMonthCodec    "firstDay"  (function SemiMonthly { firstDay = x } -> Some x | _ -> None)
        <*> jreqWith dayOfMonthCodec    "secondDay" (function SemiMonthly { secondDay = x } -> Some x | _ -> None)

    let private fortnightlyCodec =
        fun t z day -> Fortnightly { alarm = { time = t; zone = z }; day = day }
        <!> jreqWith localTimeCodec    "time" (function Fortnightly { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec "zone" (function Fortnightly { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith isoDayOfWeekCodec "day"  (function Fortnightly { day = x } -> Some x | _ -> None)

    let private weeklyCodec =
        fun t z day -> Weekly { alarm = { time = t; zone = z }; day = day }
        <!> jreqWith localTimeCodec    "time" (function Weekly { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec "zone" (function Weekly { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith isoDayOfWeekCodec "day"  (function Weekly { day = x } -> Some x | _ -> None)
        
    let private dailyCodec =
        fun t z k -> Daily { alarm = { time = t; zone = z }; kind = k }
        <!> jreqWith localTimeCodec    "time" (function Daily { alarm = { time = x } } -> Some x | _ -> None)
        <*> jreqWith dateTimeZoneCodec "zone" (function Daily { alarm = { zone = x } } -> Some x | _ -> None)
        <*> jreqWith dailyKindCodec    "kind" (function Daily { kind = x } -> Some x | _ -> None)

    let private frequencyCodec =
        fun s c -> Frequency { size = s; unit = c }
        <!> jreq                       "size" (function Frequency { size = x } -> Some x | _ -> None)
        <*> jreqWith unitOfTimeCodec   "unit" (function Frequency { unit = x } -> Some x | _ -> None)
    
    type Schedule with
        static member JsonObjCodec =
            jchoice [
                tag "frequency"     frequencyCodec
                tag "daily"         dailyCodec
                tag "weekly"        weeklyCodec
                tag "fortnightly"   fortnightlyCodec
                tag "semi-monthly"  semiMonthlyCodec
                tag "monthly"       monthlyCodec
                tag "quarterly"     quarterlyCodec
                tag "semi-annually" semiAnnuallyCodec
                tag "annually"      annuallyCodec
            ]
    
    type Alarm with
        static member JsonObjCodec =
            fun d n p s -> { domain = d; name = n; payload = p; schedule = s }
            <!> jreq    "domain"   (fun x -> Some x.domain)
            <*> jreq    "name"     (fun x -> Some x.name)
            <*> jreq    "payload"  (fun x -> Some x.payload)
            <*> jreq    "schedule" (fun x -> Some x.schedule)
                    
    type Agenda with
        static member JsonObjCodec =
            fun xs -> { alarms = xs }
            <!> jreq "alarms" (fun x -> Some x.alarms)

    let readAgenda json : Result<Agenda,_> = parseJson json

    let writeAgenda (agenda: Agenda) = toJson agenda |> string


