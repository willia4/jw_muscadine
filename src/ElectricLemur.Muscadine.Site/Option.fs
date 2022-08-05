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
