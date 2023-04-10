[<AutoOpen>]
module Archer.MicroLang.Verification

open Archer

let combineResultIgnoring defaultError a b =
    match a, b with
    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a
    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    | TestFailure tfa, TestFailure tfb -> CombinationFailure (tfa, tfb) |> TestFailure
    
let andResult = combineResultIgnoring TestSuccess

let orResult a b =
    match a, b with
    | TestFailure _, other
    | other, TestFailure _ -> other
    | _ -> a

let combineError = combineResultIgnoring TestSuccess

let expectsToBe expected result =
    if expected = result then TestSuccess
    else
        {
            Expected = $"%A{expected}"
            Actual = $"%A{result}"
        }
        |> VerificationFailure
        |> TestFailure
        
let expectsToBeWithMessage expected message result =
    let r =
        result
        |> expectsToBe expected
        
    match r with
    | TestSuccess -> r
    | TestFailure f -> FailureWithMessage (message, f) |> TestFailure

let verifyWith = expectsToBe
        
let expectsToBeTrue = expectsToBe true
       