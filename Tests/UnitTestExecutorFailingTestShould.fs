module Archer.MicroLang.Tests.``UnitTestExecutor Failing Test``

open Archer
open Archer.MicroLang

let private container = suite.Container ()
    
let ``Should return failure if the test action returns failure`` =
    container.Test (
        Setup setupExecutorFromResult,
        
        fun testCreator _ ->
            let expectedFailure = { Actual = "Things don't add up"; Expected = "nice and tidy" } |> expects.AsValidationFailure |> TestFailure
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
        Setup setupExecutorFromSetupResult,
        
        fun testBuilder _ ->
            let failure = "Setup Fail" |> expects.AsGeneralSetupTeardownFailure
            
            let executor = testBuilder (Error failure)
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> expects.ToBe (failure |> SetupExecutionFailure)
    )
    
let ``Should not call the test action if setup fails`` =
    container.Test (
        Setup setupExecutorFromTestActionAndSetupResult,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let failure = "Setup Fail" |> expects.AsGeneralSetupTeardownFailure
            
            let testAction _ _ =
                result <- expects.NotRunValidationFailure () |> TestFailure
                result
            
            let executor = testBuilder (Error failure) testAction 
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> ignore
            
            result
    )
    
let ``Test Cases`` = container.Tests