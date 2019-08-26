// Learn more about F# at http://fsharp.org
open System
open Obj

type Overs = uint8 * uint8

type Runs = 
    | Runs of int
    | Four
    | Six

type Player = string

type NotOut = Facing | NotFacing

type Wicket = 
    | Caught of Player * Player
    | CaughtAndBowled of Player
    | RunOut of Player
    | Bowled of Player
    | Stumped of Player * Player

type BallOutCome = 
    | NoBall of Runs
    | Wide of Runs
    | Runs of Runs
    | LegBye of Runs
    | Bye of Runs
    | DotBall
    | Out of Wicket

type Ball = Overs * BallOutCome

type BatsmanStatus = Out of Wicket | NotOut of NotOut

type BatsmanDetail = {
    Batsman: Player
    Runs: int
    BallsFaced: int
    Status: BatsmanStatus
}

type BowlerDetail = {
    Bowler: Player
    Runs: int
    Wickets: int
    Extras: int
    Maidens: int
}

type Team = {
    Name: string
    Players: list<Player>
}

type Innings = {
    Balls: Ball
    BattingTeam: Team
    BowlingTeam: Team
    Score: uint16
    Wickets: uint8
    Over: Overs
    FaceBatsman: BatsmanDetail
    OtherBatsman: BatsmanDetail
    Bowler: BowlerDetail
    BowlersList: Player list
    BatsmenList: Player list
}

type MatchBeforeStart = {
    Teams: ( Team * Team )
    Overs: uint8
}

type MatchOnFirstInnings = Innings

type MatchOnSecondInnings = ( Innings * Innings )

type MatchResult = 
    | WonByRuns of (Team * uint16)
    | WonByWickets of (Team * uint8)
    | Draw

type MatchCompleted = ( Innings * Innings * MatchResult)

type Match = 
    | BeforeStart of MatchBeforeStart 
    | FirstInnings of MatchOnFirstInnings 
    | SecondInnings of MatchOnSecondInnings
    | Completed of MatchCompleted
    | None

let mutable m : Match = None

type ScoreCommand = 
    | Runs of int 
    | NoBall of int
    | Wide of int

type Command = 
    | InitializeMatch
    | Exit 
    | InvalidCommand 
    | Help 
    | ScoreCommand of ScoreCommand 
    | PrintScore
    | Test

let getRuns (cmd: ScoreCommand) =
    match cmd with
    | Runs(n) -> n
    | Wide(n) | NoBall(n) -> n + 1

let getBowlerRuns (cmd: ScoreCommand) = 
    match cmd with
    | x -> getRuns x

let getBatsmanRuns (cmd: ScoreCommand) =
    match cmd with
    | Runs(n) | NoBall(n) -> n
    | Wide(n) -> 0

let getExtras (cmd: ScoreCommand) =
    match cmd with
    | Runs(n) -> 0
    | Wide(n) | NoBall(n) -> 1

let getOver (over, ball) (prevBall) (cmd: ScoreCommand) =
    match (ball, cmd) with
    | (_, NoBall(_)) | (_, Wide(_)) -> (over, ball)
    | (6, _) -> (over + 1, 0)
    | (_, _) -> (over, ball + 1)

// let updateInnings (innings: Innings) (ball: ScoreCommand) : Innings =
//     match ball with
//     | Runs(n) -> { innings with Runs = innings.Runs + n }
//     | Wide(n) -> { innings with Runs = innings.Runs + n }
//     | NoBall(n) -> { innings with Runs = innings.Runs + n }

let parseCommand (cmd: string) = 
    let tokens = cmd.Split(" ")
    match tokens with
    | [| "exit" |] -> Exit
    | [| "help" |] -> Help
    | [| "score" |] -> PrintScore
    | [| "r" ; n |] -> int n |> Runs |> ScoreCommand
    | [| "nb" |] -> NoBall 0 |> ScoreCommand
    | [| "nb" ; n |] -> int n |> NoBall |> ScoreCommand
    | [| "wd" |] -> Wide 0 |> ScoreCommand
    | [| "wd" ; n |] -> int n |> Wide |> ScoreCommand
    | [| "init" |] -> InitializeMatch
    | [| "test" |] -> Test
    | _ -> InvalidCommand

// let processScoreCommand cmd = 
//     match cmd with
//     | Runs(n) -> incrementScore n
//     | NoBall(n) | Wide(n) -> incrementScore (n + 1)

let rec readLine label =
    printf "%s : " label
    Console.ReadLine().Trim()
    |> function
        | "" -> printf "Cannot be empty, please enter some value"
                readLine label
        | s -> s

let rec readInt label =
    readLine label 
    |> Int32.TryParse
    |> function
        | (true, n) -> n
        | (false, _) -> printfn "Invalid Number"
                        readInt label

let readOptions label (options: list<'a>) : 'a =
    let optionStr i x = sprintf "Option %d : %A" (i+1) x
    let rec selectOption() =
        options |> List.tryItem ((readInt "Select Option") - 1)
                |> function
                    | Some(o) -> o
                    | None -> printfn "Invalid Option"
                              selectOption()
    options |> List.mapi optionStr |> List.iter (printfn "%s")
    printfn "%s : " label
    selectOption()

let newBatsmanDetail (player: Player) (status: BatsmanStatus) : BatsmanDetail =
        { Batsman = player; BallsFaced = 0; Runs = 0; Status = status }

let newBowlerDetail (player: Player) : BowlerDetail =
    { Bowler = player; Runs = 0; Maidens = 0; Wickets = 0; Extras = 0 }

/////////////////////////////////////////////////
// Functions that return Match
/////////////////////////////////////////////////

let newMatch() : MatchBeforeStart =
    // Functions
    let readPlayers teamSize =
        let label i = "Player " + string i
        List.map (label >> readLine) [1..teamSize]

    let readTeam label teamSize : Team =
        { Name = readLine label; Players = readPlayers teamSize }

    // Start getting input
    let teamSize = readInt "Team Size"
    let teamA = readTeam "Team A" teamSize
    let teamB = readTeam "Team B" teamSize
    let overs = readInt "Overs" |> uint8
    { Overs = overs; Teams = (teamA, teamB) }

let newMatchFirstInnings (x: MatchBeforeStart) : MatchOnFirstInnings
    printfn fst(x.Teams).Name

/////////////////////////////////////////////////
// Functions that mutate m
/////////////////////////////////////////////////
     
let initMatch () =
    match m with
    | None -> m <- BeforeStart(newMatch())
    | _ -> printfn "Already a match exists!"

let startMatch () =
    match m with
    | BeforeStart(x) -> m <- FirstInnings(newMatchFirstInnings x)
    | _ -> printfn "Match already started!"

/////////////////////////////////////////////////
// All the Print functions below this
/////////////////////////////////////////////////

let oversOfInnings (innings: Innings) =
    innings.Balls |> List.last |> fst

let oversToStr (over, ball) =
    sprintf "%d.%d" over ball

let scoreStr innings = 
    sprintf "Score: %d/%d Overs: %s" innings.Runs innings.Wickets <| ( oversOfInnings >> oversToStr ) (innings)

let printStatus() =
    match m.State with
    | NotStarted -> printfn "Match not started yet!"
    | FirstInnings -> printfn "%s" <| scoreStr m.FirstInnings
    | SecondInnings -> printfn "%s" <| scoreStr m.SecondInnings
    | Completed -> printfn "Match Finished"

let printScore() =
    match m.State with
    | NotStarted -> printfn "Match not started yet!"
    | FirstInnings -> printfn "%s" <| scoreStr m.FirstInnings
    | SecondInnings -> printfn "%s" <| scoreStr m.SecondInnings
    | Completed -> printfn "Match Finished"

let prompt() =
    match m.State with
    | NotStarted | Completed -> ">"
    | FirstInnings -> m.FirstInnings |> scoreStr
    | SecondInnings -> m.SecondInnings |> scoreStr

/////////////////////////////////////////////////
// Main Command Pattern
/////////////////////////////////////////////////

let processCommand cmd =
    match cmd with
    | InitializeMatch -> initMatch()
    | Exit -> Environment.Exit 0
    | Help -> printfn "No help for you!!!"
    | ScoreCommand(scoreCommand) -> () //processScoreCommand scoreCommand
    | PrintScore -> printScore()
    | InvalidCommand -> printfn "Invalid Command\n"
    | Test -> readOptions "Which team you support?" [TeamA; TeamB] |> ignore

/////////////////////////////////////////////////
// Entry Point
/////////////////////////////////////////////////

[<EntryPoint>]
let main argv =
    printfn "Welcome to Cricket Console!"
    while(true) do
        prompt() |> printfn "%s "
        let cmd = Console.ReadLine().Trim()
        parseCommand cmd
        |> processCommand
    0 // return an integer exit code
