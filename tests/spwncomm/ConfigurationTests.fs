module Tests.Spawn.IO.Configuration

open Xunit
open Xunit.Abstractions
open NodaTime
open FSharpPlus
open System.Diagnostics
open Spawn.IO.Configuration

let agenda = {
    alarms =
      [|{ domain = "pwc-apps.com"; name = "Tiger Atom Feed #1";
          schedule = Frequency { size = 6L; unit = UnitOfTime.PerHour };
          payload = { source = "https://gate.pform.pwc-apps.com:18151"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "word-record";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #2";
          schedule = Frequency { size = 10L; unit = UnitOfTime.Minutes };
          payload = { source = "https://gate.pform.pwc-apps.com:18152"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "client";
                      batchSize = 100; maxRetries = 1 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Daily { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                             kind = DailyKind.Weekdays };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Weekly { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                              day = IsoDayOfWeek.Friday };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Fortnightly { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                                   day = IsoDayOfWeek.Saturday };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = SemiMonthly { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                                   modifier = DayAdjustmentStrategy.WorkingDayBefore; firstDay = Day 15; secondDay = Last };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Monthly { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                               modifier = DayAdjustmentStrategy.WorkingDayBefore; day = Day 5 };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Quarterly { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                                 modifier = DayAdjustmentStrategy.Rigid; date = AnnualDate(3,31);
                                 adjustment = DateAdjustmentStrategy.Last };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = SemiAnnually { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                                    modifier = DayAdjustmentStrategy.WorkingDayClosest; date = AnnualDate(2,3);
                                    adjustment = DateAdjustmentStrategy.Specific };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } };
        { domain = "pwc-apps.com"; name = "Tiger Atom Feed #3"
          schedule = Annually { alarm = { time = LocalTime.Noon; zone = DateTimeZoneProviders.Tzdb.GetSystemDefault() };
                                modifier = DayAdjustmentStrategy.WorkingDayBefore; date = AnnualDate(4,5);
                                adjustment = DateAdjustmentStrategy.Specific };
          payload = { source = "https://gate.pform.pwc-apps.com:18153"; protocol = "https";
                      format = "xml"; feed = "atom"; recordType = "assignee";
                      batchSize = 100; maxRetries = 3 } }
      |] }

let lift = function Ok x -> x | Error e -> failwithf "%A" e

let dup num arr =
    let rec cp n xs =
        match n with
        | 0 -> xs
        | _ -> Array.append xs xs |> cp (n-1)
    cp num arr

type Serialization(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Agenda can be written to json and parsed back to self equality`` () =
        let json = writeAgenda agenda
        let a' = readAgenda json |> (function Ok x -> x | Error e -> failwithf "%A" e)
        Assert.Equal(agenda, a')

    [<Fact>]
    member __.``Large agendas of approx 5_000 alarms can be deserialized in less than 3 seconds`` () =
        let sw = Stopwatch()
        let mutable xs = agenda.alarms
        for i in 1..9 do
            xs <- dup 1 xs
            let A = { alarms = xs }

            sw.Start()
            let json = writeAgenda A
            sw.Stop()
            Assert.True(sw.ElapsedMilliseconds < 3000L, sprintf "Serializing %d items took %d ms." xs.Length sw.ElapsedMilliseconds)
            sprintf "Serializing %d items took %d ms." xs.Length sw.ElapsedMilliseconds |> output.WriteLine
            
            sw.Reset()
            sw.Start()
            let A' = readAgenda json |> (function Ok x -> x | Error e -> failwithf "%A" e)
            sw.Stop()
            Assert.Equal(A, A')
            Assert.True(sw.ElapsedMilliseconds < 3000L, sprintf "Deserializing %d items took %d ms." xs.Length sw.ElapsedMilliseconds)
            sprintf "Deserializing %d items took %d ms." xs.Length sw.ElapsedMilliseconds |> output.WriteLine
        