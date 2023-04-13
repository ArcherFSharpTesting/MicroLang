module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup should``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer.MicroLang.Types
open Archer
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``carry the result of the StartSetup event`` = 
    container.Test (fun _ ->
        let expectedFailure = "Failures abound" |> expects.AsSetupFailure |> TestFailure
        let setupPart = SetupPart (fun _ -> expectedFailure) |> Some
        let executor = buildDummyExecutor None setupPart
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (testResult, _) ->
                result <- testResult
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
        |> expects.ToBe expectedFailure
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
            | TestEndSetup (_, cancelEventArgs) ->
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
            | TestEndSetup (_, cancelEventArgs) ->
                cancelEventArgs.Cancel <- true
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> expects.ToBe (TestFailure CancelFailure)
    )
    
let ``should carry result of setup action fails`` = 
    container.Test (fun _ ->
        let expectedFailure = "This is an intended failure" |> expects.AsSetupFailure |> TestFailure
        let setupAction =
            (fun _ -> expectedFailure)
            |> SetupPart
            |> Some
            
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        let executor = buildDummyExecutor None setupAction
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestEndSetup (testResult, _) ->
                result <-
                    testResult
                    |> expects.ToBe expectedFailure
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = [
    ``carry the result of the StartSetup event``
    ``prevent the call of the test action if canceled``
    ``should cause execution to return a CancelError if canceled``
    ``should carry result of setup action fails``
]