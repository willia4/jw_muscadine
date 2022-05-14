module ElectricLemur.Muscadine.Site.Util

let appendSeqToList (a: 'a list) (b: 'a seq) =
    let rec m acc remaining =
        if (remaining |> Seq.isEmpty) then
            acc
        else
            m ((Seq.head remaining) :: a) (Seq.tail remaining)

    m (List.rev a) (b |> Seq.rev) |> List.rev