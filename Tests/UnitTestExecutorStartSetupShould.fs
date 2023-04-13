module Archer.MicroLang.Tests.``UnitTestExecutor StartSetup``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer
open Archer.MicroLang.Types
open Microsoft.FSharp.Control

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor StartSetup should")

let ``Test Cases`` = [
    container.Test ("prevent the call of the test setup if canceled", fun _ ->
        let mutable result = TestSuccess
        
        let setupPart =
            SetupPart (fun () ->
                result <- expects.NotRunValidationFailure () |> TestFailure
                TestSuccess
            )
            |> Some
            
        let executor = buildDummyExecutor None setupPart
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestSetupStarted cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test action if canceled", fun _ ->
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            TestSuccess
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestSetupStarted cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test action if failed", fun _ ->
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- expects.NotRunValidationFailure () |> TestFailure
            
            "some setup failure"
            |> expects.AsSetupFailure
            |> TestFailure
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestSetupStarted cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("should cause execution to return a CancelError if canceled", fun _ ->
        let executor = buildDummyExecutor None None
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestSetupStarted cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> expects.ToBe (TestFailure CancelFailure)
    )
]