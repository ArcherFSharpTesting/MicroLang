[<AutoOpen>]
module Archer.MicroLang.Lang

open Archer.CoreTypes
open Archer.MicroLang.Types

let suite = TestContainerBuilder ()

let randomInt _ = System.Random().Next ()
let ignoreInt _ = randomInt ()
let ignoreString _ = $"%d{randomInt ()}%d{randomInt ()}%d{randomInt ()}"

let ignorePath _ = $"%s{ignoreString ()}.test"

let successfulTest () = TestSuccess
        
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