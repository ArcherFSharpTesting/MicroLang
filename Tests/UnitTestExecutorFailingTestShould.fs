module Archer.MicroLang.Tests.``UnitTestExecutor Failing Test``

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ()
    
let ``Should return failure if the test action returns failure`` =
    container.Test (fun _ ->
        let expectedFailure = { Actual = "Things don't add up"; Expected = "nice and tidy" } |> expects.AsValidationFailure |> TestFailure
        let expectedResult = expectedFailure |> TestExecutionResult
        
        let container = suite.Container ("fake", "container")
        let test = container.Test (fun _ -> expectedFailure)
        
        let execution = test.GetExecutor () 
        
        let result =
            execution
            |> getEmptyEnvironment
            |> execution.Execute
        
        result
        |> expects.ToBe expectedResult
    )
    
let ``Should fail test if setup fails`` =
    container.Test (fun _ ->
        let failure = "Setup Fail" |> expects.AsGeneralSetupTeardownFailure
        let setup = Setup (fun () -> Error failure)
        
        let container = suite.Container ("fake", "container")
        let test = container.Test (setup, successfulEnvironmentTest)
        
        let executor = test.GetExecutor ()
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> expects.ToBe (failure |> SetupExecutionFailure)
    )
    
let ``Should not call the test action if setup fails`` =
    container.Test (fun _ ->
        let mutable result = TestSuccess
        
        let failure = "Setup Fail" |> expects.AsGeneralSetupTeardownFailure
        let setup = Setup (fun () -> Error failure)
        
        let testAction _ _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            result
        
        let container = suite.Container ("Fake", "Container")
        let test = container.Test (setup, testAction)
        let executor = test.GetExecutor ()
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = container.Tests