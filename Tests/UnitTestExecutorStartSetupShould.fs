module Archer.MicroLang.Tests.``UnitTestExecutor StartSetup should``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``prevent the call of the test setup if canceled`` =
    container.Test (
        SetupPart setupExecutorFromSetupAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let setupAction =
                (fun _ ->
                    result <- newFailure.With.TestExecutionShouldNotRunValidationFailure () |> TestFailure
                    Ok ()
                )
                
            let executor = testBuilder setupAction
            
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
    container.Test (
        SetupPart setupExecutorFromTestAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let testAction _ =
                result <- newFailure.With.TestExecutionShouldNotRunValidationFailure () |> TestFailure
                TestSuccess
                
            let executor = testBuilder testAction
            
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
    container.Test (
        SetupPart setupExecutorFromTestAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let testAction _ =
                result <- newFailure.With.TestExecutionShouldNotRunValidationFailure () |> TestFailure
                
                "Should not have been here"
                |> newFailure.With.TestOtherExpectationFailure
                |> TestFailure
                
            let executor = testBuilder testAction
            
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
    container.Test (
        SetupPart setupExecutor,
        
        fun executor _ ->
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