module Archer.MicroLang.Tests.``UnitTestExecutor With a Failing Test``

open Archer.CoreTypes
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang.Lang
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor Failing Test")
let generateFailure failureType details =
    details |> failureType |> TestFailure
    
let private dummyExecutor (testAction: (unit -> TestResult) option) (parts: TestPart option) =
    let test = buildDummyTest testAction parts
        
    test.GetExecutor ()
    
let ``Test Cases`` = [
    container.Test ("Should return failure if the test action returns failure", fun () ->
        let expectedResult = { Actual = "Things don't add up"; Expected = "nice and tidy" } |> generateFailure VerificationFailure
        let test = dummyExecutor (Some (fun () -> expectedResult)) None
        
        let result = test.Execute ()
        
        result
        |> expectsToBe expectedResult
    )
    
    container.Test ("Should raise all events even if setup fails", fun () ->
        let failure = "Setup Fail" |> generateFailure SetupFailure
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let mutable cnt = 0
        let increment _ _ = cnt <- cnt + 1
        
        test.StartExecution.AddHandler increment
        test.StartSetup.AddHandler increment
        test.EndSetup.AddHandler increment
        test.StartTest.AddHandler increment
        test.EndTest.AddHandler increment
        test.StartTearDown.AddHandler increment
        test.EndExecution.AddHandler increment
        
        test.Execute () |> ignore 
        
        cnt
        |> expectsToBe 7
    )
    
    container.Test ("Should raise all events even if setup fails", fun () ->
        let failure = "Setup Fail" |> generateFailure SetupFailure
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let result = test.Execute ()
        
        result
        |> expectsToBe failure
    )
    
    container.Test ("Should raise all events even if setup fails", fun () ->
        let failure = "Setup Fail" |> generateFailure SetupFailure
        let test = dummyExecutor None (Some (SetupPart (fun () -> failure)))
        
        let mutable result = notRunGeneralFailure
        
        let combineResult = combineResultIgnoring notRunGeneralFailure
        
        let testTheResult _ (args: obj) =
            let a =
                match args with
                | :? TestCancelEventArgsWithResults as a -> a.TestResult
                | :? TestEventArgs as b -> b.TestResult
                | _ -> failure

            let r =
                a
                |> expectsToBe failure
                |> combineResult result
                
            result <- r
            
        test.EndSetup.AddHandler testTheResult
        test.EndTest.AddHandler testTheResult
        test.EndExecution.AddHandler testTheResult
        
        test.Execute () |> ignore 
        
        result
    )
    
    container.Test ("Should raise all events even if setup fails", fun () ->
        let failure = "Setup Fail" |> generateFailure SetupFailure
        let mutable result = TestSuccess
        
        let testAction () =
            result <- notRunValidationFailure
            result
        
        let test = dummyExecutor (Some testAction) (Some (SetupPart (fun () -> failure)))
        
        test.Execute () |> ignore
        
        result
    )
]