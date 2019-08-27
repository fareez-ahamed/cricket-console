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

type TeamName = string

type Team = {
    Name: TeamName
    Players: list<Player>
}

type Innings = {
    Balls: Ball list
    BattingTeam: Team
    BowlingTeam: Team
    Score: int
    Wickets: int
    Overs: Overs
    FaceBatsman: BatsmanDetail
    OtherBatsman: BatsmanDetail
    Bowler: BowlerDetail
    BowlersList: BowlerDetail list
    BatsmenList: BatsmanDetail list
}

type MatchBeforeStart = {
    Teams: ( Team * Team )
    Overs: uint8
}

type MatchOnFirstInnings = Innings

type MatchOnSecondInnings = ( Innings * Innings )

type MatchResult = 
    | WonByRuns of (TeamName * uint16)
    | WonByWickets of (TeamName * uint8)
    | Draw

type MatchCompleted = ( Innings * Innings * MatchResult)

type Match = 
    | BeforeStart of MatchBeforeStart 
    | FirstInnings of MatchOnFirstInnings 
    | SecondInnings of MatchOnSecondInnings
    | Completed of MatchCompleted
    | BeforeInit

let mutable m : Match = BeforeInit

type ScoreCommand = 
    | Runs of int 
    | NoBall of int
    | Wide of int

type Command = 
    | InitializeMatch
    | StartMatch
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
    | [| "start" |] -> StartMatch
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
    let optionStr i x = sprintf " %d. %A" (i+1) x
    let rec selectOption() : 'a =
        options |> List.tryItem ((readInt "Select Option") - 1)
                |> function
                    | Some o -> o
                    | None -> printfn "Invalid Option"
                              selectOption()
    printfn "\n%s : " label
    options |> List.mapi optionStr |> List.iter (printfn "%s")
    selectOption()

let newBatsmanDetail (player: Player) (status: BatsmanStatus): BatsmanDetail =
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

let errorMatchNotInit() = failwith("Match not initialized yet")

let teams m =

    let teamsFromInnings i = [i.BattingTeam; i.BowlingTeam]
    match m with
    | BeforeInit -> None
    | BeforeStart m -> m.Teams |> fun (x, y) -> [x; y]  |> Some
    | FirstInnings i 
    | SecondInnings (i, _)
    | Completed (i, _, _) -> teamsFromInnings i |> Some
    |> function
       | Some l -> List.sort l |> Some
       | None -> None

let otherTeam m t =
    let other (x, y) a = if a = x then y else x
    match m with
    | BeforeInit -> None
    | BeforeStart {Teams = x} -> other x t |> Some
    | FirstInnings x
    | SecondInnings (x, _) 
    | Completed (x, _, _) -> other (x.BattingTeam, x.BowlingTeam) t |> Some

let unwrap errorMsg (x: 'a option) : 'a =
    match x with
    | Some y -> y
    | None  -> failwithf "%s" errorMsg

let newMatchFirstInnings (mbs: MatchBeforeStart) : MatchOnFirstInnings =
    let m = BeforeStart mbs
    let teamsList = teams m |> unwrap "No teams available in the match"
    let tossWonBy = teamsList |> List.map ( fun x -> x.Name ) |> readOptions "Who is batting first"
    let battingTeam =  teamsList |> List.find ( fun x -> x.Name = tossWonBy )
    let bowlingTeam = otherTeam m battingTeam
                      |> unwrap "No teams available in the match"
    let faceBatsman = readOptions "Face Batsman" battingTeam.Players
    let otherBatsman = battingTeam.Players |> List.except (seq { yield faceBatsman })
                       |> readOptions "Other Batsman"
    let bowler = readOptions "First Bowler" bowlingTeam.Players

    { 
        BattingTeam = battingTeam; 
        BowlingTeam = bowlingTeam;
        FaceBatsman =  Facing |> NotOut |> newBatsmanDetail faceBatsman;
        OtherBatsman = NotFacing |> NotOut |> newBatsmanDetail otherBatsman ;
        Bowler = newBowlerDetail bowler;
        Score = 0;
        Wickets = 0;
        Balls = [];
        Overs = (0, 0);
        BowlersList = [];
        BatsmenList = [];
    }

/////////////////////////////////////////////////
// Functions that mutate m
/////////////////////////////////////////////////
     
let initMatch () =
    match m with
    | BeforeInit -> m <- BeforeStart(newMatch())
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

let scoreStr (innings: Innings) = 
    sprintf "Score: %d/%d Overs: %s" innings.Score innings.Wickets <| ( oversOfInnings >> oversToStr ) (innings)


let prompt() =
    match m with
    | _ -> ">"

/////////////////////////////////////////////////
// Main Command Pattern
/////////////////////////////////////////////////

let processCommand cmd =
    match cmd with
    | InitializeMatch -> initMatch()
    | StartMatch -> startMatch()
    | Exit -> Environment.Exit 0
    | Help -> printfn "No help for you!!!"
    | ScoreCommand(scoreCommand) -> () //processScoreCommand scoreCommand
    | PrintScore -> ()
    | InvalidCommand -> printfn "Invalid Command\n"
    | Test -> ()

/////////////////////////////////////////////////
// Entry Point
/////////////////////////////////////////////////

[<EntryPoint>]
let main argv =
    printfn "Welcome to Cricket Console!"
    while(true) do
        prompt() |> printf "%s "
        let cmd = Console.ReadLine().Trim()
        parseCommand cmd
        |> processCommand
    0 // return an integer exit code
