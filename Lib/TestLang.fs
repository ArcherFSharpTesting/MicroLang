[<AutoOpen>]
module Archer.MicroLang.Lang

open Archer.CoreTypes
open Archer.MicroLang.Types

let suite = TestContainerBuilder ()

let randomInt _ = System.Random().Next ()
let ignoreInt _ = randomInt ()
let ignoreString _ = $"%d{randomInt ()}%d{randomInt ()}%d{randomInt ()}"

let ignorePath _ = $"%s{ignoreString ()}.test"

let combineResultIgnoring defaultError a b =
    match a, b with
    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a
    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    | TestFailure tfa, TestFailure tfb -> CombinationFailure (tfa, tfb) |> TestFailure

let combineError = combineResultIgnoring TestSuccess

let successfulTest () = TestSuccess

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
        
let expectsToBeTrue result =
    if result then TestSuccess
    else
        {
            Expected = true.ToString ()
            Actual = result.ToString ()
        }
        |> VerificationFailure
        |> TestFailure
        
let buildDummyTest (testAction: (unit -> TestResult) option) (parts: TestPart option) =
    let c = suite.Container (ignoreString (), ignoreString ())
        
    match parts, testAction with
    | None, None -> c.Test (ignoreString (), successfulTest, EmptyPart, ignoreString (), ignoreInt ())
    | None, Some action -> c.Test (ignoreString (), action, EmptyPart, ignoreString (), ignoreInt ())
    | Some part, None -> c.Test (ignoreString (), successfulTest, part, ignoreString (), ignoreInt ())
    | Some part, Some action -> c.Test (ignoreString (), action, part, ignoreString (), ignoreInt ())
    
let buildDummyExecutor (testAction: (unit -> TestResult) option) (parts: TestPart option) =
    let test = buildDummyTest testAction parts
    
    test.GetExecutor ()
    
let notRunGeneralFailure = "Not Run" |> GeneralFailure |> TestFailure

let notRunExpectation = { Expected = "Not to have been run"; Actual = "Was run" } |> VerificationFailure

let notRunValidationFailure = notRunExpectation |> TestFailure