module Seq

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

  let copyChunk (chunk: seq<'a>) =
    System.Collections.Immutable.ImmutableList.Empty.AddRange(chunk)

  for item in s do
      if (predicate item) then
          if (chunk.Count > 0) then
              yield (copyChunk chunk)
          chunk.Clear()
      else
        chunk.Add(item)

  if (chunk.Count > 0) then
    yield (copyChunk chunk)
}