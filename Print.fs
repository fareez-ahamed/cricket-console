module Print

open Match

let oversOfInnings (innings: Innings) =
    innings.Balls |> List.last |> fst

let oversToStr (over, ball) =
    sprintf "%d.%d" over ball

let scoreStr (innings: Innings) = 
    sprintf "Score: %d/%d Overs: %s" innings.Score innings.Wickets <| ( oversOfInnings >> oversToStr ) (innings)


let prompt() =
    match m with
    | _ -> ">"