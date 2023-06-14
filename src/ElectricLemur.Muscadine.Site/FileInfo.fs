module FileInfo
open System
open System.IO
open System.Threading
open Microsoft.AspNetCore.Http
open CommunityToolkit.HighPerformance
open System.Collections.Immutable

type FileInfo =
| PathFile of string
| HttpFile of IFormFile
| BytesFile of string * ImmutableArray<byte>

let ofHttpFormFile (f: IFormFile) = HttpFile(f)
let ofPath (p: string) = PathFile(p)
let ofBytes (fileName: string) (bytes: ImmutableArray<byte>) = BytesFile(fileName, bytes)

let isValid f =
    match f with
    | PathFile fullPath -> System.IO.File.Exists(fullPath)
    | HttpFile _ -> true // no way to check that the IFormFile is still valid so assume it hasn't been disposed
    | BytesFile _ -> true // we have the bytes so it's definitionally valid
 
let fileName f =
    match f with
    | PathFile fullPath -> System.IO.Path.GetFileName(fullPath)
    | HttpFile file -> file.FileName
    | BytesFile (fileName, _) -> fileName
    
let fileExtension f =
    match f with
    | PathFile fullPath -> System.IO.Path.GetExtension(fullPath)
    | HttpFile file -> System.IO.Path.GetExtension(file.FileName)
    | BytesFile (fileName, _) -> System.IO.Path.GetExtension(fileName)
    
let copyToStream (target: System.IO.Stream) f =
    match f with
    | PathFile fullPath -> task {
            use f = System.IO.File.OpenRead(fullPath)
            do! f.CopyToAsync(target)
        }
    | HttpFile file -> task {
            do! file.CopyToAsync(target)
        }
    | BytesFile (_, bytes) -> task {
            use bufferStream = bytes.AsMemory().AsStream()
            do! bufferStream.CopyToAsync(target)
        }

let getBytes f =
    match f with
    | PathFile fullPath -> task {
            use f = System.IO.File.OpenRead(fullPath)
            use ms = new MemoryStream(if f.Length > Int32.MaxValue then Int32.MaxValue else (int f.Length))
            do! f.CopyToAsync(ms)
            return ms.ToArray().ToImmutableArray()
        }
    | HttpFile f -> task {
            use ms = new MemoryStream(if f.Length > Int32.MaxValue then Int32.MaxValue else (int f.Length))
            do! f.CopyToAsync(ms)
            return ms.ToArray().ToImmutableArray()
        }
    | BytesFile (_, bytes) -> Task.fromResult bytes
    
let clone f =
    match f with
    | BytesFile _ -> Task.fromResult f
    | _ -> task {
        let! bytes = getBytes f
        let fileName = fileName f
        return BytesFile (fileName, bytes)
    }
    
/// <summary>
/// Creates a readable Stream from a FileInfo. 
/// </summary>
/// <remarks>The returned Stream must be Disposed by the caller</remarks>
let rec toStream f =
    match f with
    | PathFile fullPath -> Task.fromResult (System.IO.File.OpenRead(fullPath) :> System.IO.Stream)
    | HttpFile _ -> task {
            let! cloned = clone f
            return! toStream cloned // this is only not infinitely recursive because clone will never return an HttpFile
        }
    | BytesFile (_, bytes) -> task {
            let ms = new MemoryStream(bytes.Length)
            use sourceStream = bytes.AsMemory().AsStream()
            do! sourceStream.CopyToAsync(ms)
            ms.Seek(0L, SeekOrigin.Begin)
            return (ms :> System.IO.Stream) 
        }
        