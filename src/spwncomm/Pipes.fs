namespace Spawn

open Messages
open FSharpPlus
open System.IO.Compression
open System.IO.Pipes
open System.Text
open System.Threading
open System

module IO =
    
    let _1KB_  = 1024
    let _4KB_  = _1KB_ * 4
    let _64KB_ = _1KB_ * 64
    let _1MB_  = _1KB_ * _1KB_
    let _10MB_ = _1MB_ * 10
    
    let private TimeOut = 30
    let private _30_SEC_ = 30_000

    exception PipeNotConnected
    exception PipeCannotWrite

    // Server: Read => Decompress => Deserialize => Process => Serialize => Compress => Write
    // Server.HandleMessage<'Request, 'Response>: CancellationToken(30 seconds) -> unit
    //    Read:        CancellationToken -> Result<Compressed, Exception>
    //    Decompress:  Compressed -> Result<Serialized, Exception>
    //    Deserialize: Serialized -> Result<'Request, Exception>
    //    Process:     'Request -> Result<'Response, Exception>
    //    Serialize:   'Response -> Result<Serialized, Exception>
    //    Compress:    Serialized -> Result<Compressed, Exception>
    //    Write:       Compressed -> Result<unit, Exception>
    
    // Client: Serialize => Compress => Write => Read => Decompress => Deserialize
    // Client.ExecuteRequest<'Request, 'Response>: Request => CancellationToken => Result<'Response, Exception>
    //    Serialize:   'Request -> Result<Serialized, Exception>
    //    Compress:    Serialized -> Result<Compressed, Exception>
    //    Write:       Compressed -> Result<unit, Exception>
    //    Read:        CancellationToken -> Result<Compressed, Exception>
    //    Decompress:  Compressed -> Result<Serialized, Exception>
    //    Deserialize: Serialized -> Result<'Response, Exception>
    
    let private readMessageAsync (pipe: PipeStream) (token: CancellationToken) =
        let buffer : byte [] = Array.zeroCreate _4KB_
        let builder = StringBuilder(_64KB_, _10MB_)
        let cts = CancellationTokenSource.CreateLinkedTokenSource(token)
        cts.CancelAfter(_30_SEC_)
        
        let rec readLoopAsync (stream: PipeStream) (token: CancellationToken)
                              (buf: byte[]) (sb: StringBuilder) isComplete =
            async {
                try
                    if isComplete then
                        return sb.ToString().Trim('\x00') |> Ok
                    else
                        let! bytesRead = stream.ReadAsync(buf.AsMemory(), token).AsTask() |> Async.AwaitTask
                        let chunk = Encoding.UTF8.GetString(ReadOnlySpan(buf, 0, bytesRead))
                        let isComplete' = chunk.EndsWith("\x00\x00", StringComparison.Ordinal)
                        sb.Append(ReadOnlySpan(chunk.ToCharArray())) |> ignore
                        return! readLoopAsync stream token buf sb isComplete'
                with ex -> return ex.ToString() |> Error
            }
        readLoopAsync pipe cts.Token buffer builder false
        
    let private writeMessageAsync (pipe: PipeStream) (token: CancellationToken) (message: string) =
        let endMessage = Encoding.UTF8.GetBytes([|'\x00';'\x00'|]) |> ReadOnlyMemory
        let bytes = Encoding.UTF8.GetBytes(message)
        if not pipe.IsConnected then raise PipeNotConnected
        elif not pipe.CanWrite then raise PipeCannotWrite
        else
            try
                let cts = CancellationTokenSource.CreateLinkedTokenSource(token)
                cts.CancelAfter(_30_SEC_)
                async {
                    do! pipe.WriteAsync(ReadOnlyMemory(bytes), cts.Token).AsTask() |> Async.AwaitTask
                    do! pipe.WriteAsync(endMessage, cts.Token).AsTask() |> Async.AwaitTask
                    do! pipe.FlushAsync(cts.Token) |> Async.AwaitTask
                    pipe.WaitForPipeDrain()
                } |> Async.RunSynchronously |> Ok
            with ex -> sprintf "%A" ex |> Error

    let compress (message: string) =
        let msg = message |> Encoding.UTF8.GetBytes
        let maxSize = BrotliEncoder.GetMaxCompressedLength(Seq.length msg)
        let compressed : byte [] = Array.zeroCreate maxSize
        let bytesWritten = ref 0
        let quality = 5
        let defaultWindowBits = 22  // https://github.com/dotnet/corefx/blob/master/src/System.IO.Compression.Brotli/src/System/IO/Compression/BrotliUtils.cs
        match BrotliEncoder.TryCompress(ReadOnlySpan(msg), Span(compressed),
                                        bytesWritten, quality, defaultWindowBits) with
        | false -> "Failed to compress message" |> Error
        | true  -> compressed |> Convert.ToBase64String |> Ok
        
    let decompress message =
        let decompressed : byte [] = Array.zeroCreate _10MB_
        let msg = message |> Convert.FromBase64String
        match BrotliDecoder.TryDecompress(ReadOnlySpan(msg), Span(decompressed)) with
        | false, _           -> "Failed to decompress message" |> Error
        | true, bytesWritten -> Encoding.UTF8.GetString(ReadOnlySpan(decompressed, 0, bytesWritten)) |> Ok

                
    type SpawnServer(pipeName) =

        let pipe = new NamedPipeServerStream(pipeName, PipeDirection.InOut, 1,
                                             PipeTransmissionMode.Byte,
                                             PipeOptions.WriteThrough ||| PipeOptions.Asynchronous)
        
        member this.GetRequestAsync(token) =
            async {
                if not pipe.IsConnected then
                    printfn "Waiting for connection..."
                    do! pipe.WaitForConnectionAsync(token) |> Async.AwaitTask
                    pipe.GetImpersonationUserName() |> printfn "Connected as %s"
                let! request = readMessageAsync pipe token
                return request >>= decompress >>= (Request.Deserialize)
            }
        
        member this.SendResponseAsync(response: Response, token) =
            async {
                return response.Serialize() >>= compress >>= writeMessageAsync pipe token
            }
        
        interface IDisposable with
            member this.Dispose() = pipe.Dispose()
        
    
    type SpawnClient(serverName, pipeName) =
        
        let pipe = new NamedPipeClientStream(serverName, pipeName, PipeDirection.InOut,
                                             PipeOptions.Asynchronous ||| PipeOptions.WriteThrough)
    
        member this.SendRequestAsync(request: Request, token: CancellationToken) =
            async {
                if not pipe.IsConnected then
                    do! pipe.ConnectAsync(TimeOut, token) |> Async.AwaitTask
                return request.Serialize() >>= compress >>= writeMessageAsync pipe token
            }
            
        member this.GetResponseAsync(token) =
            async {
                let! response = readMessageAsync pipe token
                return response >>= decompress >>= (Response.Deserialize)
            }
            
        interface IDisposable with
            member this.Dispose() = pipe.Dispose()
    