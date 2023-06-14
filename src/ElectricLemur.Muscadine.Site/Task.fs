module ElectricLemur.Muscadine.Site.Task
  let map (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a>) = task {
    let! v = t
    return (f v)
  }

  let bind (f: 'a -> System.Threading.Tasks.Task<'b>) (t: System.Threading.Tasks.Task<'a>) = task {
    let! v = t
    return! (f v)
  }

  let completedTask x =(fun _ -> System.Threading.Tasks.Task.CompletedTask) x

  let fromResult x = System.Threading.Tasks.Task.FromResult(x)
