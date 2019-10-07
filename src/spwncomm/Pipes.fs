namespace Spawn

open FsToolkit.ErrorHandling
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
        let log = Spawn.Logging.Log("Spawn.IO.Pipes")
        
        type Compressed = Compressed of string
        type Serialized = Serialized of string
        type Message<'T> = Message of 'T
            
        let private readAsync stream cancelToken =
            let cts = CancellationTokenSource.CreateLinkedTokenSource([|cancelToken|])

            let buffer : byte [] = Array.zeroCreate _8KB_
            let builder = StringBuilder(_64KB_, _10MB_)
            let rec readLoopAsync (ps: PipeStream) token buf (sb: StringBuilder) isComplete =
                async {
                    try
                        match isComplete with
                        | true  -> return sb.ToString().Trim('\x00') |> Compressed |> Ok
                        | false ->
                            let! bytesRead = ps.ReadAsync(buf.AsMemory(), token).AsTask() |> Async.AwaitTask
                            let chunk = Encoding.UTF8.GetString(ReadOnlySpan(buf, 0, bytesRead))
                            sb.Append(ReadOnlySpan(chunk.ToCharArray())) |> ignore
                            let isComplete' = chunk.EndsWith("\x00\x00", StringComparison.Ordinal)
                            return! readLoopAsync ps token buf sb isComplete'
                    with ex -> return Error ex
                }
            readLoopAsync stream cts.Token buffer builder false |> Async.RunSynchronously
            
        let private decompress compressed =
            try
                let (Compressed message) = compressed
                let decompressed : byte [] = Array.zeroCreate _10MB_
                let msg = message |> Convert.FromBase64String
                
                use decoder = new BrotliDecoder()
                match decoder.Decompress(ReadOnlySpan(msg), Span(decompressed)) with
                | OperationStatus.Done, _, written ->
                    Encoding.UTF8.GetString(ReadOnlySpan(decompressed, 0, written))
                    |> Serialized
                    |> Ok
                    
                | status, consumed, written ->
                    let msg = (status.ToString(), consumed, written)
                              |||> sprintf "[%s] Failed to decompress the message. %d bytes consumed and %d bytes written"
                    match status with
                    | OperationStatus.InvalidData -> InvalidDataException(msg) :> exn |> Error
                    | _ -> IOException(msg) :> exn |> Error

            with ex -> Error ex
            
        let inline private deserializeMessage (deserializer: string -> 'T) serialized =
            try
                let (Serialized contents) = serialized
                let pipeline = deserializer >> Message >> Ok
                contents |> pipeline
            with ex -> Error ex
            
        let inline private handleRequest (handler: 'Request -> 'Response) request =
            try
                let (Message req) = request
                let pipeline = handler >> Message >> Ok
                req |> pipeline
            with ex -> Error ex
            
        let inline private serializeMessage (serializer: 'T -> string) message =
            try
                let (Message msg) = message
                let pipeline = serializer >> Serialized >> Ok
                msg |> pipeline
            with ex -> Error ex
        
        let private compress serialized =
            try
                let (Serialized contents) = serialized
                let msg = Encoding.UTF8.GetBytes(contents)
                let maxSize = BrotliEncoder.GetMaxCompressedLength(Seq.length msg)
                let compressed : byte [] = Array.zeroCreate maxSize
                let consumed = ref 0
                let written = ref 0
                                    
                use encoder = new BrotliEncoder(5, 22)
                match encoder.Compress(ReadOnlySpan(msg), Span(compressed), consumed, written, true) with
                | OperationStatus.Done ->
                    let pipeline = Convert.ToBase64String >> Compressed >> Ok
                    pipeline compressed
                    
                | status ->
                    let message = (status.ToString(), !consumed, !written)
                                  |||> sprintf "[%s]! Failed to compress the message. %d bytes consumed and %d bytes written"
                    match status with
                    | OperationStatus.InvalidData -> InvalidDataException(message) :> exn |> Error
                    | _ -> IOException(message) :> exn |> Error

            with ex -> Error ex
        
        let private writeAsync (stream: PipeStream) cancelToken compressed =
            let cts = CancellationTokenSource.CreateLinkedTokenSource([|cancelToken|])

            let msgEnd = Encoding.UTF8.GetBytes([|'\x00';'\x00'|]) |> ReadOnlyMemory
            let msg = compressed |> function Compressed x -> x
            let bytes = Encoding.UTF8.GetBytes(msg)
            if not stream.IsConnected then Error PipeNotConnected
            elif not stream.CanWrite then Error PipeCannotWrite
            else
                async {
                    try
                        do! stream.WriteAsync(ReadOnlyMemory(bytes), cts.Token).AsTask() |> Async.AwaitTask
                        do! stream.WriteAsync(msgEnd, cts.Token).AsTask() |> Async.AwaitTask
                        do! stream.FlushAsync(cts.Token) |> Async.AwaitTask
                        stream.WaitForPipeDrain()
                        return Ok ()
                    with ex -> return Error ex
                } |> Async.RunSynchronously

        type SpawnServer(pipeName, ?token: CancellationToken) =
            let log = Logging.Log("Spawn.IO.Pipes.SpawnServer")
            
            let pipe = new NamedPipeServerStream(pipeName, PipeDirection.InOut, 1,
                                                 PipeTransmissionMode.Byte,
                                                 PipeOptions.WriteThrough ||| PipeOptions.Asynchronous)
            
            let serverCancel = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource([|t|]))
                                     |> Option.defaultValue (new CancellationTokenSource())
            
            member this.StartAsync(handler, serializer, deserializer) =
                async {
                    while not serverCancel.IsCancellationRequested do
                        use opCancel = CancellationTokenSource.CreateLinkedTokenSource([|serverCancel.Token|])
                        
                        if not pipe.IsConnected then
                            "Waiting for connection..." |> log.Info
                            do! pipe.WaitForConnectionAsync(opCancel.Token) |> Async.AwaitTask
                            opCancel.CancelAfter(DefaultTimeout)
                            pipe.GetImpersonationUserName() |> sprintf "Connected as %s" |> log.Info
                        
                        try
                            readAsync pipe opCancel.Token
                            |> Result.bind decompress
                            |> Result.bind (deserializeMessage deserializer)
                            |> Result.bind (handleRequest handler)
                            |> Result.bind (serializeMessage serializer)
                            |> Result.bind compress
                            |> Result.bind (writeAsync pipe opCancel.Token)
                            |> ignore
                        
                        finally
                            pipe.Disconnect()
                            "Disconnected" |> log.Info
                }
                    
                
            interface IDisposable with
                member this.Dispose() =
                    "Disposing SpawnServer" |> log.Debug
                    serverCancel.Cancel()
                    pipe.Dispose()
                    serverCancel.Dispose()
                    "SpawnServer disposed" |> log.Debug
        
        type SpawnClient(serverName, pipeName, ?token: CancellationToken) =
            
            let pipe = new NamedPipeClientStream(serverName, pipeName, PipeDirection.InOut,
                                                 PipeOptions.Asynchronous ||| PipeOptions.WriteThrough)
            
            let clientCancel = token |> Option.map (fun t -> CancellationTokenSource.CreateLinkedTokenSource([|t|]))
                                     |> Option.defaultValue (new CancellationTokenSource())
            
            member this.SendAsync(request, serializer, deserializer) =
                clientCancel.CancelAfter(DefaultTimeout)
                
                async {
                    if not pipe.IsConnected then
                        do! pipe.ConnectAsync(clientCancel.Token) |> Async.AwaitTask
                    
                    request
                    |> Result.Ok
                    |> Result.map Message
                    |> Result.bind (serializeMessage serializer)
                    |> Result.bind compress
                    |> Result.bind (writeAsync pipe clientCancel.Token)
                    |> ignore
                    
                    return
                        readAsync pipe clientCancel.Token
                        |> Result.bind decompress
                        |> Result.bind (deserializeMessage deserializer)
                        |> Result.bind (function Message x -> Ok x)
                }
            
            interface IDisposable with
                member this.Dispose() =
                    clientCancel.Cancel()
                    pipe.Dispose()
                    clientCancel.Dispose()
