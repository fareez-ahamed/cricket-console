// Learn more about F# at http://fsharp.org

open System

type WicketType = 
    | Caught
    | BowledOut 
    | CaughtAndBowled

type ScoreCommand = Run of int | NoBall | Wide

type Command = 
    | InitializeMatch
    | Exit 
    | InvalidCommand 
    | Help 
    | ScoreCommand of ScoreCommand 
    | PrintScore

let mutable score = 0
let mutable teamA = ""
let mutable teamB = ""
let mutable matchStarted = false

let incrementScore n =
    score <- score + n

let parseCommand (cmd: string) = 
    match cmd with
    | "exit" -> Exit
    | "help" -> Help
    | "score" -> PrintScore
    | "nb" -> ScoreCommand(NoBall)
    | "wd" -> ScoreCommand(Wide)
    | "init" -> InitializeMatch
    | _ -> InvalidCommand

let processScoreCommand cmd = 
    match cmd with
    | Run(n) -> incrementScore(n)
    | NoBall | Wide -> incrementScore(1)

let initMatch() =
    Console.Write("Team A : ")
    teamA <- Console.ReadLine().Trim()
    Console.Write("Team B : ")
    teamB <- Console.ReadLine().Trim()
    matchStarted <- true

let processCommand cmd =
    match cmd with
    | InitializeMatch -> initMatch()
    | Exit -> Environment.Exit 0
    | Help -> printfn "No help for you!!!"
    | ScoreCommand(scoreCommand) -> processScoreCommand scoreCommand
    | PrintScore -> printfn "Score: %d" score
    | InvalidCommand -> printfn "Invalid Command\n"

[<EntryPoint>]
let main argv =
    printfn "Welcome to Cricket Console!"
    while(true) do
        Console.Write "> "
        let cmd = Console.ReadLine().Trim()
        parseCommand cmd
        |> processCommand
    0 // return an integer exit code
