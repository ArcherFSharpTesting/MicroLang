module Archer.MicroLang.Tests.``UnitTestExecutor happy path``

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.FrameworkTypes
open Archer.MicroLang
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``Should have the creating test as its parent`` =
    container.Test(fun _ ->
        let container = suite.Container (ignoreString (), ignoreString ())
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        executor.Parent
        |> expects.ToBe test
    )
    
let ``Should return success if test action returns success`` =
    container.Test (fun _ ->
        let container = suite.Container ("Fake", "Container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> expects.ToBe (TestSuccess |> TestExecutionResult)
    )
    
let ``Should raise all events in correct order`` =
    container.Test(fun _ ->
        let container = suite.Container ("fake", "container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        let mutable cnt = 0
        let notRun = expects.GeneralNotRunFailure () |> TestFailure
        let mutable result = notRun
        
        let combineResult = combineResultIgnoring notRun
            
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStartExecution _ ->
                let r =
                    cnt
                    |> expects.ToBe 0
                    |> withMessage "Start Execution out of order"
                
                cnt <- cnt + 1
                result <-
                    r |> combineResult result
            | TestStartSetup _ ->
                let r =
                    cnt
                    |> expects.ToBe 1
                    |> withMessage "Start Setup out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
            | TestEndSetup _ ->
                let r =
                    cnt
                    |> expects.ToBe 2
                    |> withMessage "End Setup out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
            | TestStart _ ->
                let r =
                    cnt
                    |> expects.ToBe 3
                    |> withMessage "Start Test out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
            | TestEnd _ ->
                let r =
                    cnt
                    |> expects.ToBe 4
                    |> withMessage "End Test out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
            | TestStartTeardown ->
                let r =
                    cnt
                    |> expects.ToBe 5
                    |> withMessage "Start Tear Down out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
            | TestEndExecution _ ->
                let r =
                    cnt
                    |> expects.ToBe 6
                    |> withMessage "End Execution out of order"
                    
                cnt <- cnt + 1
                result <- r |> combineResult result
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = container.Tests