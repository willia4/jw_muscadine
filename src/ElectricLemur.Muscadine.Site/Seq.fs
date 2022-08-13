module Seq

let copyToImmutableSeq (s: seq<'a>) = List.ofSeq s |> Seq.ofList

// Returns a new Sequence with the values of b prepended to the values of a
let prepend a b = Seq.append b a

// Returns a new Sequence with item appended to l if the predicate is true. Otherwise, returns l.
let appendIf p item s =
  if p then
    Seq.append s [ item ]
  else
    s

/// Divides a Sequence into chunks, starting a new chunk
/// when the predicate returns true for an item in the Sequence
/// The item that caused the predicate to return true is not returned.
/// Returns a Sequence of chunks, with each chunk being a non-empty Sequence
let chunkByPredicate (predicate: 'a -> bool)  (s: seq<'a>) = seq {
  let chunk = new System.Collections.Generic.List<'a>()

  for item in s do
      if (predicate item) then
          if (chunk.Count > 0) then
              yield (copyToImmutableSeq chunk)
          chunk.Clear()
      else
        chunk.Add(item)

  if (chunk.Count > 0) then
    yield (copyToImmutableSeq chunk)
}

let flatten (s: seq<seq<'a>>) = seq {
  for outer in s do
    yield! outer
}

let mapAsync (mapper: 'a -> System.Threading.Tasks.Task<'b>) (s: seq<'a>) = task {
  let results = new System.Collections.Generic.List<'b>()

  for i in s do
    let! r = mapper i
    results.Add(r)

  return (copyToImmutableSeq results)
}

let filterAsync (predicate: 'a -> System.Threading.Tasks.Task<bool>) (s: seq<'a>) = task {
  let results = new System.Collections.Generic.List<'a>()

  for i in s do
    let! p = predicate i
    if p then
      results.Add(i)

  return (copyToImmutableSeq results)
}

let iterAsync (f: 'a -> System.Threading.Tasks.Task<unit>) (s: seq<'a>) = task {
  for i in s do
      do! (f i)
}