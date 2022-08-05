module List

/// Returns a new List with the values of b appended to the values of a
let appendSeq (a: 'a list) (b: 'a seq) =
  List.append a (b |> List.ofSeq)

// Returns a new List with the values of b prepended to the values of a
let prepend a b = List.append b a

// Returns a new List with item appended to l if the predicate is true. Otherwise, returns l.
let appendIf predicate item l =
  if predicate then
    List.append l [ item ]
  else
    l