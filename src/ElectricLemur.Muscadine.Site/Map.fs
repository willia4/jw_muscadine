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
