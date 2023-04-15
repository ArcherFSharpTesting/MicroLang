module Archer.MicroLang.Tests.``UnitTestExecutor StartSetup should``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer
open Archer.MicroLang.Types
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``prevent the call of the test setup if canceled`` =
    container.Test (fun _ ->
        let mutable result = TestSuccess
        
        let setupPart =
            Setup (fun _ ->
                result <- expects.NotRunValidationFailure () |> TestFailure
                Ok ()
            )
            
        let container = suite.Container ("Fake", "Container")
        let test = container.Test (setupPart, successfulEnvironmentTest)
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
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
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            TestSuccess
            
        let container = suite.Container ("fake", "container")
        let test = container.Test testAction
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``prevent the call of the test action if failed`` = 
    container.Test (fun _ ->
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            
            "Should not have been here"
            |> expects.AsOtherTestExecutionFailure
            |> TestFailure
            
        let container = suite.Container ("Fake", "Container")
        let test = container.Test testAction
            
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
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
        let container = suite.Container ("fake", "container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> expects.ToBe (GeneralCancelFailure |> GeneralExecutionFailure)
    )
    
let ``Test Cases`` = container.Tests