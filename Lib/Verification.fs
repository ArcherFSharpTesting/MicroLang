[<AutoOpen>]
module Archer.MicroLang.Verification

open Archer
open Archer.MicroLang.VerificationTypes
        
let expects = Expect ()

let newFailure = FailureBuilder ()

let combineResultIgnoring defaultError a b =
    match a, b with
    | TestFailure TestCanceledFailure as failure, _
    | _, (TestFailure TestCanceledFailure as failure) -> failure

    | TestIgnored _ as ing, _ -> ing

    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a

    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    
    | TestFailure tfa, TestFailure tfb -> CombinationFailure (tfa, tfb) |> TestFailure
    | TestFailure _ as failure, _
    | _, (TestFailure _ as failure) -> failure
    
let andResult = combineResultIgnoring TestSuccess

let orResult a b =
    match a, b with
    | TestFailure _, other
    | other, TestFailure _ -> other
    | _ -> a

let combineError = combineResultIgnoring TestSuccess
        
let withMessage message result =
    match result with
    | TestFailure f -> FailureWithMessage (message, f) |> TestFailure
    | TestSuccess
    | TestIgnored _
    | TestFailure _ -> result