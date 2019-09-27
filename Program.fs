// Learn more about F# at http://fsharp.org
open System
open Match

type Command = 
    | InitializeMatch
    | StartMatch
    | Exit 
    | InvalidCommand 
    | Help 
    | ScoreCommand of ScoreCommand 
    | PrintScore
    | Test

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


/////////////////////////////////////////////////
// Main Command Pattern
/////////////////////////////////////////////////

let processCommand cmd =
    match cmd with
    | InitializeMatch -> init()
    | StartMatch -> start()
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
        Print.prompt() |> printf "%s "
        let cmd = Console.ReadLine().Trim()
        parseCommand cmd
        |> processCommand
    0 // return an integer exit code
