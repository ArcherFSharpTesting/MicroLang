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
            SetupPart (fun () ->
                result <- expects.NotRunValidationFailure () |> TestFailure
                TestSuccess
            )
            |> Some
            
        let executor = buildDummyExecutor None setupPart
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
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
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``prevent the call of the test action if failed`` = 
    container.Test (fun _ ->
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
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``should cause execution to return a CancelError if canceled`` = 
    container.Test (fun _ ->
        let executor = buildDummyExecutor None None
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartSetup cancelEventArgs ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> expects.ToBe (TestFailure CancelFailure)
    )
    
let ``Test Cases`` = container.Tests