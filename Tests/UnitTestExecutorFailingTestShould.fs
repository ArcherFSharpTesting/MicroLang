module Archer.MicroLang.Tests.``UnitTestExecutor With a Failing Test``

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor Failing Test")

let private dummyExecutor (testAction: (FrameworkEnvironment -> TestResult) option) (parts: TestPart option) =
    let test = buildDummyTest testAction parts
        
    test.GetExecutor ()
    
let ``Test Cases`` = [
    container.Test ("Should return failure if the test action returns failure", fun _ ->
        let expectedResult = { Actual = "Things don't add up"; Expected = "nice and tidy" } |> expects.AsValidationFailure |> TestFailure
        let test = dummyExecutor (Some (fun _ -> expectedResult)) None
        
        let result =
            test
            |> getNoFrameworkInfoFromExecution
            |> test.Execute
        
        result
        |> expects.ToBe expectedResult
    )
    
    container.Test ("Should raise all events even if setup fails", fun _ ->
        let failure = "Setup Fail" |> expects.AsSetupFailure |> TestFailure
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let mutable cnt = 0
        
        test.TestLifecycleEvent
        |> Event.add (fun _ ->
            cnt <- cnt + 1
        )
        
        test
        |> getNoFrameworkInfoFromExecution
        |> test.Execute
        |> ignore 
        
        cnt
        |> expects.ToBe 7
    )
    
    container.Test ("Should raise all events even if setup fails", fun _ ->
        let failure = "Setup Fail" |> expects.AsSetupFailure |> TestFailure
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let result =
            test
            |> getNoFrameworkInfoFromExecution
            |> test.Execute
        
        result
        |> expects.ToBe failure
    )
    
    container.Test ("Should raise all events even if setup fails", fun _ ->
        let failure = "Setup Fail" |> expects.AsSetupFailure |> TestFailure
        
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let mutable cnt = 0
        
        test.TestLifecycleEvent
        |> Event.add (fun _ ->
            cnt <- cnt + 1
        )
        
        test
        |> getNoFrameworkInfoFromExecution
        |> test.Execute
        |> ignore 
        
        cnt
        |> expects.ToBe 7
    )
    
    container.Test ("Should raise all events even if setup fails", fun _ ->
        let failure = "Setup Fail" |> expects.AsSetupFailure |> TestFailure
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            result
        
        let test = dummyExecutor (Some testAction) (Some (SetupPart (fun () -> failure)))
        
        test
        |> getNoFrameworkInfoFromExecution
        |> test.Execute
        |> ignore
        
        result
    )
]