module Map

/// Applies a key-generating function to each element of a list and yields a Map
/// based on the item keys. Each value in the map is a List of items matching that key.
let ofGroupedList (keyProjection: 'a -> string) (valueProjection: 'a -> 'c) elements =
  elements
  |> List.map (fun i -> keyProjection i, valueProjection i)
  |> List.groupBy fst
  |> Map.ofList
  |> Map.map (fun _ l -> List.map snd l)

/// Takes an existing Map and returns a new Map contains all of the original data as well as
/// a default value for any keys which were not in the original Map
let withPlaceholderIds (keys: seq<'Key>) (defaultValue: 'T) (map: Map<'Key, 'T>) =
  keys
  |> Seq.fold (fun m k ->
    match Map.containsKey k m with
    | true -> m
    | false -> Map.add k defaultValue m
  ) map

/// Merges Map m2 into Map m1.
/// If m1 does not contain a key from m2, the resulting map contains the value from m2.
/// If m1 already contains a key from m2, the value for that key in
/// the resulting map is the output of the merger function with applied with the
/// value from m1 and then the value from m2
let merge (merger: 'k -> 'v -> 'v -> 'v) (m1: Map<'k, 'v>) (m2: Map<'k, 'v>) =
  m2
    |> Map.fold (fun res k m2Value ->
        res
        |> Map.change k (fun m1Value ->
            match m1Value with
            | Some m1Value -> Some (merger k m1Value m2Value)
            | None -> Some m2Value)
      ) m1