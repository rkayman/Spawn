namespace SpawnTimers

module Actor =

    open System.Threading
    open System

    type Actor<'a> = MailboxProcessor<'a>
        
    type Event<'a> = 
        | Next of 'a
        | Error of exn
        | Completed

    type Command<'a> = 
        | Publish of 'a
        | Subscribe of 'a Event Actor
        | Unsubscribe of 'a Event Actor
        | Dispose

    type Observable<'a> = {
        subscribe: 'a Event Actor -> unit
        unsubscribe: 'a Event Actor -> unit
        abort: unit -> unit
        dispose: unit -> unit
    }

    module HotObservable =

        type State<'a> = { generator: IDisposable option; subscribers: Map<int, 'a Event Actor> }

        let publish (xs: _ Event Actor list) (msg: _ Event) = xs |> Seq.iter (fun x -> x.Post(msg))

        let dispose (x: IDisposable option) = if x.IsSome then x.Value.Dispose()

        let private processor (generator: Actor<_> -> IDisposable)
                              (inbox: Actor<_>) = 
            let rec loop state = async {
                let! msg = inbox.Receive()
                match msg with
                | Publish evt -> 
                    let subscribers = state.subscribers |> Map.toList |> List.map snd
                    Next evt |> publish subscribers
                    return! loop state

                | Subscribe actor -> 
                    let hash = actor.GetHashCode()
                    let subscribers' = state.subscribers |> Map.add hash actor
                    match state.generator with
                    | Some _ -> return! loop { state with subscribers = subscribers' }
                    | None   -> return! loop { generator = Some (generator inbox); subscribers = subscribers' }

                | Unsubscribe actor ->
                    let hash = actor.GetHashCode()
                    let subs' = state.subscribers |> Map.remove hash
                    match subs'.Count, state.generator with
                    | 0, Some gen -> gen.Dispose(); return! loop { generator = None; subscribers = subs' }
                    | _, _        -> return! loop { state with subscribers = subs' }

                | Dispose -> dispose state.generator
            }
            loop { generator = None; subscribers = Map.empty }

        let generate generator = 
            let cts = new CancellationTokenSource()
            let actor = Actor.Start(processor generator, cts.Token)

            let disposable = { new IDisposable with member this.Dispose() = actor.Post(Dispose); (actor :> IDisposable).Dispose() }
            { subscribe     = (fun x -> actor.Post(Subscribe x)); 
              unsubscribe   = (fun x -> actor.Post(Unsubscribe x));
              abort         = cts.Cancel;
              dispose       = disposable.Dispose }
