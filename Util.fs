module Util

let unwrap errorMsg (x: 'a option) : 'a =
    match x with
    | Some y -> y
    | None  -> failwithf "%s" errorMsg