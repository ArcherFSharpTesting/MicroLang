module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup should``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer.MicroLang.Types
open Archer
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``carry the result of the StartSetup event`` = 
    container.Test (fun _ ->
        let container = suite.Container ("My Fake", "Conainer")
        
        let expectedFailure = "Failures abound" |> expects.AsGeneralSetupTeardownFailure
        let setupPart = Setup (fun _ -> (expectedFailure |> Error))
        
        let test = container.Test (setupPart, successfulEnvironmentTest)
        let executor = test.GetExecutor ()
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (testResult, _) ->
                result <-
                    testResult
                    |> expects.ToBe (expectedFailure |> SetupFailure)
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``prevent the call of the test action if canceled`` =
    container.Test (fun _ ->
        let container = suite.Container ("Fake", "Container")
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            TestSuccess
            
        let test = container.Test testAction
            
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (_, cancelEventArgs) ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
        
let ``should cause execution to return a CancelError if canceled`` = 
    container.Test (fun _ ->
        let container = suite.Container ("Fake", "Container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (_, cancelEventArgs) ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> expects.ToBe (GeneralCancelFailure |> GeneralExecutionFailure)
    )
    
let ``should carry result of setup action fails`` = 
    container.Test (fun _ ->
        let expectedFailure = "This is an intended failure" |> expects.AsGeneralSetupTeardownFailure
        let setupResult = expectedFailure  |> Error
        let setupAction = Setup (fun _ -> setupResult)
        
        let container = suite.Container ("My Fake", "Container")
        let test = container.Test (setupAction, successfulEnvironmentTest)
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (testResult, _) ->
                result <-
                    testResult
                    |> expects.ToBe (expectedFailure |> SetupFailure)
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = container.Tests