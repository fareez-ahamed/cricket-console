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
    Status: BatsmanStatus
}

type BowlingDetail = {
    Bowler: Player
    Runs: int
    Wickets: int
    Extras: int
    Maiden: int
}

type Innings = {
    Balls: list<Ball>
    Runs: int
    Wickets: int
    Batting: list<BatsmanDetail>
    Bowling: list<BowlingDetail>
}

type MatchState = NotStarted | FirstInnings | SecondInnings | Completed

type Team = {
    Name: string
    Players: list<Player>
}

type BattingTeam = TeamA | TeamB

type Match = {
    mutable BattingTeam: BattingTeam
    mutable Overs: int
    mutable TeamA: Team
    mutable TeamB: Team
    mutable FirstInnings: Innings
    mutable SecondInnings: Innings
    mutable State: MatchState
}

let mutable thisMatch : Match = {
    BattingTeam = TeamA
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
        Balls = []
        Runs = 0
        Wickets = 0
        Batting = []
        Bowling = []
    }
    SecondInnings = {
        Balls = []
        Runs = 0
        Wickets = 0
        Batting = []
        Bowling = []
    }
    State = NotStarted
}

type ScoreCommand = 
    | Run of int 
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

let incrementScore n =
    thisMatch.State
    |> function
        | NotStarted -> ()
        | FirstInnings -> 
            thisMatch.FirstInnings <- 
                { thisMatch.FirstInnings with Runs = thisMatch.FirstInnings.Runs + n }
        | SecondInnings -> 
            thisMatch.SecondInnings <- 
                { thisMatch.SecondInnings with Runs = thisMatch.SecondInnings.Runs + n }
        | Completed -> ()

let parseCommand (cmd: string) = 
    let tokens = cmd.Split(" ")
    match tokens with
    | [| "exit" |] -> Exit
    | [| "help" |] -> Help
    | [| "score" |] -> PrintScore
    | [| "nb" |] -> ScoreCommand( NoBall 0 )
    | [| "nb" ; n |] -> ScoreCommand( NoBall <| int n )
    | [| "wd" |] -> ScoreCommand( Wide 0 )
    | [| "wd" ; n |] -> ScoreCommand( Wide <| int n )
    | [| "init" |] -> InitializeMatch
    | [| "test" |] -> Test
    | _ -> InvalidCommand

let processScoreCommand cmd = 
    match cmd with
    | Run(n) -> incrementScore(n)
    | NoBall(n) | Wide(n) -> incrementScore(n + 1)

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

let readOptions (options: list<'a>) : 'a =
    let optionStr i x = sprintf "Option %d : %A" (i+1) x
    let rec selectOption() =
        options |> List.tryItem ((readInt "Select Option") - 1)
                |> function
                    | Some(o) -> o
                    | None -> printfn "Invalid Option"
                              selectOption()
    options |> List.mapi optionStr |> List.iter (printfn "%s")
    selectOption()
     
let initMatch() =
    let readPlayers() =
        let label i = "Player " + string i
        List.map (label >> readLine) [1..11]
    thisMatch.TeamA <- { Name = readLine "Team A"; Players = readPlayers() }
    thisMatch.TeamB <- { Name = readLine "Team B"; Players = readPlayers() }
    thisMatch.State <- FirstInnings

let printScore() =
    let scoreStr innings = sprintf "%d/%d" innings.Runs innings.Wickets
    match thisMatch.State with
    | NotStarted -> printfn "Match not started yet!"
    | FirstInnings -> printfn "%s" <| scoreStr thisMatch.FirstInnings
    | SecondInnings -> printfn "%s" <| scoreStr thisMatch.SecondInnings
    | Completed -> printf "Match Finished"

let processCommand cmd =
    match cmd with
    | InitializeMatch -> initMatch()
    | Exit -> Environment.Exit 0
    | Help -> printfn "No help for you!!!"
    | ScoreCommand(scoreCommand) -> processScoreCommand scoreCommand
    | PrintScore -> printScore()
    | InvalidCommand -> printfn "Invalid Command\n"
    | Test -> printf "%A" <| readOptions([TeamA; TeamB])

[<EntryPoint>]
let main argv =
    printfn "Welcome to Cricket Console!"
    while(true) do
        Console.Write "> "
        let cmd = Console.ReadLine().Trim()
        parseCommand cmd
        |> processCommand
    0 // return an integer exit code
