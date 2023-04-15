module Archer.MicroLang.Tests.``UnitTestExecutor StartExecution should``

open Archer.MicroLang.Types
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ()

let ``prevent the call of the test setup if canceled`` =
    container.Test (
        Setup setupExecutorFromSetupAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let setupAction =
                (fun _ ->
                    result <- expects.NotRunValidationFailure () |> TestFailure
                    Ok ()
                )
                
            let executor = testBuilder setupAction
            
            executor.TestLifecycleEvent
            |> Event.add (fun args ->
                match args with
                | TestStartExecution cancelEventArgs ->
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
        Setup setupExecutorFromTestAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let testAction _ =
                result <- expects.NotRunValidationFailure () |> TestFailure
                TestSuccess
                
            let executor = testBuilder testAction
            
            executor.TestLifecycleEvent
            |> Event.add (fun args ->
                match args with
                | TestStartExecution cancelEventArgs ->
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
        Setup setupExecutor,
        
        fun executor _ ->
            executor.TestLifecycleEvent
            |> Event.add (fun args ->
                match args with
                | TestStartExecution cancelEventArgs ->
                    cancelEventArgs.Cancel <- true
                | _ -> ()
            )
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> expects.ToBe (GeneralCancelFailure |> GeneralExecutionFailure)
    )
    
let ``Test Cases`` = container.Tests