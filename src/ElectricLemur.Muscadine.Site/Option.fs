module Option

let ofResult (r: Result<'a, 'e>) =
  match r with
  | Ok v -> Some v
  | Error _ -> None

/// Takes a Sequence of Options and returns Some of a Sequence of unwrapped values
/// iff all Options in the Sequence were Some.
/// If any Option in the Sequence was None, None is returned
let unwrapSeqElements (s: seq<option<'a>>) =
    s
    |> Seq.fold (fun (acc: option<System.Collections.Generic.List<'a>>) next ->
      match acc, next with
      | None, _ -> None
      | _, None -> None
      | Some acc, Some v ->
        acc.Add(v) |> ignore
        Some acc
      ) (Some (new System.Collections.Generic.List<'a>()))
    |> Option.map (fun l -> l :> seq<'a>)

/// Takes a List of Options and returns Some of a List of unwrapped values
/// iff all Options in the List were Some.
/// If any Option in the List was None, None is returned
let unwrapListElements (s: List<option<'a>>) =
  unwrapSeqElements s
  |> Option.map List.ofSeq

/// Given a Sequence of Options, returns the first Option that has Some value. Returns None if all Options in the Sequence are None.
let choose (options: seq<option<'a>>) =
  options
  |> Seq.tryFind Option.isSome
  |> Option.flatten

/// Applies a sequence of functions to a single Option and returns the first result from those applications that is Some.
/// Returns None if none of the function applications return Some.
/// Note: does not short-circuit; all functions will be executed
let choosef (fs: seq<('a option -> 'b option)>) (o: 'a option) =
    fs
    |> Seq.map (fun f -> f o)
    |> choose