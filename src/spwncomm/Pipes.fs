namespace Spawn

open Spawn.Logging
open FSharpPlus
open System.Buffers
open System.IO
open System.IO.Compression
open System.IO.Pipes
open System.Text
open System.Threading
open System

module IO =
    let _1KB_  = 1024
    let _8KB_  = _1KB_ * 8
    let _64KB_ = _1KB_ * 64
    let _1MB_  = _1KB_ * _1KB_
    let _10MB_ = _1MB_ * 10
    
    let private DefaultTimeout = TimeSpan.FromSeconds(10.)

    exception PipeNotConnected
    exception PipeCannotWrite
    
    module Pipes =
        type Compressed = Compressed of string

        type Serialized = Serialized of string

        type PipeMessage<'T> = PipeMessage of 'T
        
        type Sent = Sent

        let private readAsync stream cancelToken onComplete =
            let cts = CancellationTokenSource.CreateLinkedTokenSource([|cancelToken|])

            let buffer : byte [] = Array.zeroCreate _8KB_
            let builder = StringBuilder(_64KB_, _10MB_)
            let rec readLoopAsync (ps: PipeStream) token buf (sb: StringBuilder) isComplete =
                async {
                    try
                        match isComplete with
                        | true  ->
                            return sb.ToString().Trim('\x00') |> Compressed |> onComplete |> Ok
                        | false ->
                            let! bytesRead = ps.ReadAsync(buf.AsMemory(), token).AsTask() |> Async.AwaitTask
                            let chunk = Encoding.UTF8.GetString(ReadOnlySpan(buf, 0, bytesRead))
                            sb.Append(ReadOnlySpan(chunk.ToCharArray())) |> ignore
                            let isComplete' = chunk.EndsWith("\x00\x00", StringComparison.Ordinal)
                            return! readLoopAsync ps token buf sb isComplete'
                    with ex -> return Error ex
                }
            readLoopAsync stream cts.Token buffer builder false
            
        let private decompress event =
            let (Compressed body) = event.body
            let decompressed : byte [] = Array.zeroCreate _10MB_
            let body' = body |> Convert.FromBase64String
            
            use decoder = new BrotliDecoder()
            match decoder.Decompress(ReadOnlySpan(body'), Span(decompressed)) with
            | OperationStatus.Done, _, written ->
                Encoding.UTF8.GetString(ReadOnlySpan(decompressed, 0, written))
                |> Serialized
                |> nextEvent event "Decompressed"
                
            | status, consumed, written ->
                let msg = (status.ToString(), consumed, written)
                          |||> sprintf "[%s] Failed to decompress the message. %d bytes consumed and %d bytes written"
                match status with
                | OperationStatus.InvalidData -> InvalidDataException(msg) |> raise //:> exn |> Error
                | _ -> IOException(msg) |> raise //:> exn |> Error
            
        let private deserializeMessage deserialize event =
            let (Serialized body) = event.body
            let pipeline = deserialize >> PipeMessage >> (nextEvent event "Deserialized")
            body |> pipeline
            
        let private handleRequest (handler: 'Request -> 'Response) event =
            let (PipeMessage msg) = event.body
            let pipeline = handler >> PipeMessage >> (nextEvent event "ResponseCreated")
            msg |> pipeline
            
        let private serializeMessage serialize event =
            let (PipeMessage msg) = event.body
            let pipeline = serialize >> string >> Serialized >> (nextEvent event "Serialized")
            msg |> pipeline
        
        let private compress event =
            let (Serialized body) = event.body
            let msg = Encoding.UTF8.GetBytes(body)
            let maxSize = BrotliEncoder.GetMaxCompressedLength(Seq.length msg)
            let compressed : byte [] = Array.zeroCreate maxSize
            let consumed = ref 0
            let written = ref 0
                                
            use encoder = new BrotliEncoder(5, 22)
            match encoder.Compress(ReadOnlySpan(msg), Span(compressed), consumed, written, true) with
            | OperationStatus.Done ->
                let pipeline = Convert.ToBase64String >> Compressed >> (nextEvent event "Compressed")
                pipeline compressed
                
            | status ->
                let message =
                    (status.ToString(), !consumed, !written)
                    |||> sprintf "[%s]! Failed to compress the message. %d bytes consumed and %d bytes written"
                match status with
                | OperationStatus.InvalidData -> InvalidDataException(message) |> raise
                | _ -> IOException(message) |> raise
        
        let private writeAsync (stream: PipeStream) cancelToken event =
            async {
                let cts = CancellationTokenSource.CreateLinkedTokenSource([|cancelToken|])

                let (Compressed body) = event.body
                let msgEnd = Encoding.UTF8.GetBytes([|'\x00';'\x00'|]) |> ReadOnlyMemory
                let bytes = Encoding.UTF8.GetBytes(body)
                
                if not stream.IsConnected then raise PipeNotConnected
                if not stream.CanWrite then raise PipeCannotWrite
                
                do! stream.WriteAsync(ReadOnlyMemory(bytes), cts.Token).AsTask() |> Async.AwaitTask
                do! stream.WriteAsync(msgEnd, cts.Token).AsTask() |> Async.AwaitTask
                do! stream.FlushAsync(cts.Token) |> Async.AwaitTask
                return nextEvent event "Sent" Sent
            }
            

        type SpawnPipeHelper(?log: Logging.Log, ?token: CancellationToken) =
            let pipeCancel = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource([|t|]))
                                   |> Option.defaultValue (new CancellationTokenSource())
                                   
            let defaultLog g f = log |> Option.map f |> Option.defaultValue g
            
            let defaultLogUnit = defaultLog (fun _ -> ())
            
            member this.TokenSource with get() = pipeCancel
            
            member this.LogInfo = defaultLogUnit (fun x -> x.Info)
            member this.LogDebug = defaultLogUnit (fun x -> x.Debug)
            member this.LogError(ex) = ex |> sprintf "%A" |> defaultLogUnit (fun x -> x.Error)
            member this.CancelAfter(timespan: TimeSpan) =
                pipeCancel.CancelAfter(timespan)
                timespan.Seconds |> sprintf "Set to cancel after %d seconds" |> this.LogInfo

                
        type SpawnServer(pipeName, logMgr, ?token: CancellationToken) as self =
            inherit SpawnPipeHelper(Logging.Log(logMgr, "Spawn.IO.Pipes.SpawnServer"), ?token = token)
            
            let pipe = new NamedPipeServerStream(pipeName, PipeDirection.InOut, 1,
                                                 PipeTransmissionMode.Byte,
                                                 PipeOptions.WriteThrough ||| PipeOptions.Asynchronous)
                
            let waitForConnectionAsync (opCancel: CancellationTokenSource) =
                async {
                    let sessionId = Guid.NewGuid()
                    if pipe.IsConnected then pipe.Disconnect()

                    let session = newEvent sessionId "ServerConnecting" "Waiting for connection..."
                    session.ToString() |> self.LogInfo
                    do! pipe.WaitForConnectionAsync(opCancel.Token) |> Async.AwaitTask
                    opCancel.CancelAfter(DefaultTimeout)
                            
                    return pipe.GetImpersonationUserName() |> sprintf "Server connected (%s)"
                           |> nextEvent session "ServerConnected" 
                }
            
            let readRequestAsync = readAsync pipe
            
            let sendResponseAsync = writeAsync pipe
            
            let logResult f x =
                let ans = f x
                ans.ToString() |> self.LogInfo
                ans
            
            member this.ListenAsync(handle, serialize: 'b -> string, deserialize) =
                async {
                    while not self.TokenSource.IsCancellationRequested do
                        use opCancel = CancellationTokenSource.CreateLinkedTokenSource([|self.TokenSource.Token|])
                        
                        try
                            try
                                let! session = waitForConnectionAsync opCancel
                                session.ToString() |> this.LogInfo
                                
                                let onComplete = nextEvent session "RequestReceived"
                                let! request = readRequestAsync opCancel.Token onComplete
                                let response =
                                    request
                                    |> logResult (Result.either id raise)
                                    |> logResult (decompress)
                                    |> logResult (deserializeMessage deserialize)
                                    |> logResult (handleRequest handle)
                                    |> logResult (serializeMessage serialize)
                                    |> logResult (compress)
                                
                                let! sent = sendResponseAsync opCancel.Token response
                                sent.ToString() |> this.LogInfo

                            with ex -> this.LogError ex
                        finally
                            pipe.Disconnect()
                            this.LogInfo "Disconnected"
                }
            
            interface IDisposable with
                member this.Dispose() =
                    self.LogDebug "Disposing SpawnServer"
                    self.TokenSource.Cancel()
                    pipe.Dispose()
                    self.TokenSource.Dispose()
                    self.LogDebug "SpawnServer disposed"
        
        type SpawnClient(serverName, pipeName, ?token: CancellationToken) as self =
            inherit SpawnPipeHelper(?token = token)
            
            let pipe = new NamedPipeClientStream(serverName, pipeName, PipeDirection.InOut,
                                                 PipeOptions.Asynchronous ||| PipeOptions.WriteThrough)
            
            let ensureConnectedAsync (token: CancellationToken) =
                async {
                    if not pipe.IsConnected then
                        self.LogInfo "Client not connected. Connecting now..."
                        do! pipe.ConnectAsync(token) |> Async.AwaitTask
                    
                    self.LogInfo "Client connected"
                }
                
            let prepareRequest serialize activity name command =
                try
                    command
                    |> PipeMessage
                    |> newEvent activity name
                    |> serializeMessage serialize
                    |> compress
                    |> Ok
                with ex -> Error ex
                
            let sendRequestAsync = writeAsync pipe self.TokenSource.Token
            
            let waitForResponseAsync = readAsync pipe self.TokenSource.Token
            
            let unwrapResponse event =
                let (PipeMessage body) = event.body
                body
            
            member this.SendAsync(command, activity, name, serialize, deserialize) =
                async {
                    this.CancelAfter(DefaultTimeout)
                    do! ensureConnectedAsync self.TokenSource.Token

                    let request = command
                                  |> prepareRequest serialize activity name
                                  |> Result.either id raise
                    let! sent   = request |> sendRequestAsync
                    
                    let onComplete = nextEvent sent "MessageRead"
                    let! compressed = waitForResponseAsync onComplete
                    let response =
                        compressed
                        |> Result.either id raise
                        |> decompress
                        |> deserializeMessage deserialize
                        |> unwrapResponse
                    
                    return response
                }
            
            interface IDisposable with
                member this.Dispose() =
                    self.TokenSource.Cancel()
                    pipe.Dispose()
                    self.TokenSource.Dispose()
