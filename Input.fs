module Input

open System

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