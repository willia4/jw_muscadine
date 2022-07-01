module Id

open System
open System.Linq
open System.Text.RegularExpressions

let private compressedIdRegex = new Regex "[0-9a-fA-F]{32}"
let private expansionRegex = new Regex("([0-9a-fA-F]{8})([0-9a-fA-F]{4})([0-9a-fA-F]{4})([0-9a-fA-F]{4})([0-9a-fA-F]{12})")

let isGuid (s: string) =
    match Guid.TryParse(s.Trim()) with
    | true, _ -> true
    | false, _ -> false

let isCompressedId (s: string) = 
    let isValid (s: string) = compressedIdRegex.IsMatch(s.Trim())

    s.Length = 32 && isValid s

let isId s = isGuid s || isCompressedId s

let compressId (id: string) = id.ToLowerInvariant().Replace("-", "")

let expandId s = 
    if isGuid s then
        Some (Guid.Parse(s) |> string)
    elif isCompressedId s then
        let m = expansionRegex.Match(s.Trim())
        if m.Success then
            let groups = m.Groups |> Seq.skip 1 |> Seq.map (fun g -> g.Value) |> Seq.toArray

            let id = $"{groups.[0]}-{groups.[1]}-{groups.[2]}-{groups.[3]}-{groups.[4]}"
            if isGuid id then 
                Some id
            else
                None
        else
            None
    else
        None