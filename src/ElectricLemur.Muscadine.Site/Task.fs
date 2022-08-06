
module Task

  let bindSeq (f: 'a -> System.Threading.Tasks.Task<'b>) (t: System.Threading.Tasks.Task<'a seq>) = task {
    let! values = t

    let results = new System.Collections.Generic.List<'b>()

    for v in values do
      let! mappedValue = f v
      results.Add(mappedValue)

    let results = results :> seq<'b>
    return results
  }
