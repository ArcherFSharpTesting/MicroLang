module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer.MicroLang.Types
open Archer
open Microsoft.FSharp.Control

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor EndSetup should")

let ``Test Cases`` = [
    container.Test ("carry the result of the StartSetup event", fun _ ->
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
    
    container.Test ("prevent the call of the test action if canceled", fun _ ->
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
    
    container.Test ("should cause execution to return a CancelError if canceled", fun _ ->
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
    
    container.Test ("should carry result of setup action fails", fun _ ->
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
]