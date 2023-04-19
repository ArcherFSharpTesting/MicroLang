[<AutoOpen>]
module Archer.MicroLang.Verification

open Archer
open Archer.MicroLang.VerificationTypes
        
let expects = Expect ()

let newFailure = FailureBuilder ()

let combineResultIgnoring defaultError a b =
    match a, b with
    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a

    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    
    | TestFailure (TestExpectationFailure (tfa, locationA)), TestFailure (TestExpectationFailure (tfb, locationB)) ->
        let location = [locationA; locationB] |> List.min
        (CombinationFailure (tfa, tfb), location) |> TestExpectationFailure |> TestFailure
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
    | TestFailure (TestExpectationFailure (f, location)) -> (FailureWithMessage (message, f), location) |> TestExpectationFailure |> TestFailure
    | TestSuccess
    | TestFailure _ -> result
    
let by f (check: IEventChecker) =
    f () |> ignore
    if check.IsValid then
        TestSuccess
    else
        check.FailureDescription
        |> newFailure.With.TestValidationFailure (check.SuccessDescription, check.FullPath, check.LineNumber)
        |> TestFailure
    