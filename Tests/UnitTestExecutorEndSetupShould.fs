module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup should``

open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Archer
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``carry the result of the StartSetup event`` = 
    container.Test (
        Setup setupExecutorFromSetupResult,
        fun testBuilder _ ->
            let expectedFailure = "Failures abound" |> newFailure.With.GeneralSetupTeardownFailure
            
            let executor = testBuilder (expectedFailure |> Error)
            
            let mutable result = newFailure.With.GeneralNotRunFailure () |> TestFailure
            
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
    container.Test (
        Setup setupExecutorFromTestAction,
        
        fun testBuilder _ ->
            let mutable result = TestSuccess
            
            let testAction _ =
                result <- newFailure.With.NotRunValidationFailure () |> TestFailure
                TestSuccess
                
            let executor = testBuilder testAction
            
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
    container.Test (
        Setup setupExecutor,
        
        fun executor _ ->
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
    container.Test (
        Setup setupExecutorFromSetupResult,
        
        fun testBuilder _ ->
            let expectedFailure = "This is an intended failure" |> newFailure.With.GeneralSetupTeardownFailure
            let setupResult = expectedFailure  |> Error
            
            let mutable result = newFailure.With.GeneralNotRunFailure () |> TestFailure
            
            let executor = testBuilder setupResult
            
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