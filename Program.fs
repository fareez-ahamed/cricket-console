// Learn more about F# at http://fsharp.org
open System
open Obj

type Overs = int * int

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

type BattingTeam = TeamA | TeamB | NotSet

type Innings = {
    Balls: list<Ball>
    Runs: int
    Wickets: int
    Batting: list<BatsmanDetail>
    Bowling: list<BowlerDetail>
    BattingTeam: BattingTeam
}

type MatchState = NotStarted | FirstInnings | SecondInnings | Completed

type Team = {
    Name: string
    Players: list<Player>
}

type Match = {
    TeamSize: int
    Overs: int
    TeamA: Team
    TeamB: Team
    FirstInnings: Innings
    SecondInnings: Innings
    State: MatchState
}

let mutable m : Match = {
    TeamSize = 11
    Overs = 0
    TeamA = {
        Name = ""
        Players = []
    }
    TeamB = {
        Name = ""
        Players = []
    }
    FirstInnings = {
        BattingTeam = NotSet
        Balls = []
        Runs = 0
        Wickets = 0
        Batting = []
        Bowling = []
    }
    SecondInnings = {
        BattingTeam = NotSet
        Balls = []
        Runs = 0
        Wickets = 0
        Batting = []
        Bowling = []
    }
    State = NotStarted
}

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

// let incrementScore n =
//     thisMatch.State
//     |> function
//         | NotStarted -> ()
//         | FirstInnings -> 
//             thisMatch.FirstInnings <- 
//                 { thisMatch.FirstInnings with Runs = thisMatch.FirstInnings.Runs + n }
//         | SecondInnings -> 
//             thisMatch.SecondInnings <- 
//                 { thisMatch.SecondInnings with Runs = thisMatch.SecondInnings.Runs + n }
//         | Completed -> ()

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

let updateInnings (innings: Innings) (ball: ScoreCommand) : Innings =
    match ball with
    | Runs(n) -> { innings with Runs = innings.Runs + n }
    | Wide(n) -> { innings with Runs = innings.Runs + n }
    | NoBall(n) -> { innings with Runs = innings.Runs + n }

let parseCommand (cmd: string) = 
    let tokens = cmd.Split(" ")
    match tokens with
    | [| "exit" |] -> Exit
    | [| "help" |] -> Help
    | [| "score" |] -> PrintScore
    | [| "r" ; n |] -> ScoreCommand( Runs <| int n )
    | [| "nb" |] -> ScoreCommand( NoBall 0 )
    | [| "nb" ; n |] -> ScoreCommand( NoBall <| int n )
    | [| "wd" |] -> ScoreCommand( Wide 0 )
    | [| "wd" ; n |] -> ScoreCommand( Wide <| int n )
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
    printf "%s : " label
    selectOption()

let readTossOutcome() = 
    readOptions "Who is batting first?" [ TeamA; TeamB ]

let newBatsmanDetail (player: Player) (status: BatsmanStatus) : BatsmanDetail =
        { Batsman = player; BallsFaced = 0; Runs = 0; Status = status }

let readPlayer label team : Player =
    match team with
    | TeamA -> readOptions label m.TeamA.Players
    | TeamB -> readOptions label m.TeamB.Players
    | NotSet -> failwith("Team not set")
    
let readBatsman label =
    match m.State with
    | NotStarted -> failwith "Match not started"
    | FirstInnings -> readPlayer label m.FirstInnings.BattingTeam
    | SecondInnings -> readPlayer label m.SecondInnings.BattingTeam
    | Completed -> failwith "Match completed"

let readBowler label =
    match m.State with
    | NotStarted -> failwith "Match not started"
    | FirstInnings -> readPlayer label m.SecondInnings.BattingTeam
    | SecondInnings -> readPlayer label m.FirstInnings.BattingTeam
    | Completed -> failwith "Match completed"

let readBatsmanDetail = readBatsman >> newBatsmanDetail

let getBowlingTeam innings =
    match innings.BattingTeam with
    | TeamA -> TeamB
    | TeamB -> TeamA
    | NotSet -> NotSet

let newBowlerDetail (player: Player) : BowlerDetail =
    { Bowler = player; Runs = 0; Maidens = 0; Wickets = 0; Extras = 0 }
     
let initMatch() =

    let readPlayers teamSize =
        let label i = "Player " + string i
        List.map (label >> readLine) [1..teamSize]

    let readTeam label teamSize : Team =
        { Name = readLine label; Players = readPlayers teamSize }

    let firstBatsman() = readBatsmanDetail "Facing Batsman" (NotOut(Facing))
    let secondBatsman() = readBatsmanDetail "Other Batsman" (NotOut(NotFacing))
    let firstBowler() = getBowlingTeam m.FirstInnings |> readPlayer "Bowler" |> newBowlerDetail

    m <- { m with TeamSize = readInt "Team Size" }
    m <- { m with TeamA = readTeam "Team A" m.TeamSize }
    m <- { m with TeamB = readTeam "Team B" m.TeamSize }
    m <- { m with Overs = readInt "Overs" }
    m <- { m with State = FirstInnings }
    m <- { m with FirstInnings = { m.FirstInnings with BattingTeam = readTossOutcome()
                                                       Batting = firstBatsman() :: secondBatsman() :: m.FirstInnings.Batting
                                                       Bowling = firstBowler() :: m.FirstInnings.Bowling } }
    m <- { m with SecondInnings = { m.SecondInnings with BattingTeam = getBowlingTeam m.FirstInnings } }

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
    | Test -> printf "%A" <| readOptions "Which team you support?" [TeamA; TeamB]

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
