
module Task

  let map (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a>) = task {
    let! v = t
    return (f v)
  }

  let bind (f: 'a -> System.Threading.Tasks.Task<'b>) (t: System.Threading.Tasks.Task<'a>) = task {
    let! v = t
    return! (f v)
  }

  let bindSeq (f: 'a -> System.Threading.Tasks.Task<'b>) (t: System.Threading.Tasks.Task<'a seq>) = task {
    let! values = t

    let results = new System.Collections.Generic.List<'b>()

    for v in values do
      let! mappedValue = f v
      results.Add(mappedValue)

    let results = results :> seq<'b>
    return results
  }

  let mapSeq (f: 'a -> 'b) (t: System.Threading.Tasks.Task<seq<'a>>) = task {
    let! values = t
    return (values |> Seq.map f)
  }

  let mapList (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a list>) = task {
    let! values = t
    return (values |> List.map f)
  }