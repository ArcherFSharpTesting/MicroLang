module Archer.MicroLang.Tests.``UnitTestExecutor Failing Test``

open Archer
open Archer.MicroLang

let private container = suite.Container ()
    
let ``Should return failure if the test action returns failure`` =
    container.Test (
        SetupPart setupExecutorFromResult,
        
        fun testCreator _ ->
            let expectedFailure =
                { new IVerificationInfo with
                    member _.Actual with get () = "Things don't add up"
                    member _.Expected with get () = "nice and tidy"
                } |> newFailure.With.TestValidationFailure |> TestFailure
                
            let expectedResult = expectedFailure |> TestExecutionResult
            
            let execution = testCreator expectedFailure 
            
            let result =
                execution
                |> getEmptyEnvironment
                |> execution.Execute
            
            result
            |> expects.ToBe expectedResult
    )
    
let ``Should fail test if setup fails`` =
    container.Test (
        SetupPart setupExecutorFromSetupResult,
        
        fun testBuilder _ ->
            let failure = "Setup Fail" |> newFailure.With.SetupTeardownGeneralFailure
            
            let executor = testBuilder (Error failure)
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> expects.ToBe (failure |> SetupExecutionFailure)
    )
    
let ``Should not call the test action if setup fails`` =
    container.Test (
        SetupPart setupExecutorFromTestActionAndSetupResult,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let failure = "Setup Fail" |> newFailure.With.SetupTeardownGeneralFailure
            
            let testAction _ _ =
                result <- newFailure.With.TestExecutionShouldNotRunValidationFailure () |> TestFailure
                result
            
            let executor = testBuilder (Error failure) testAction 
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> ignore
            
            result
    )
    
let ``Test Cases`` = container.Tests