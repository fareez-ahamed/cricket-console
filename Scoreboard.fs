module Obj

type ScoreBoard(teamA: string, teamB: string) =

    let mutable teamA = teamA
    let mutable teamB = teamB
    let mutable score = 0

    member s.TeamA with get() = teamA and set v = teamA <- v
    member s.TeamB with get() = teamB and set v = teamB <- v
    member s.Score with get() = score and set v = score <- v

    member s.IncrementScore n =
        s.Score <- s.Score + n