module Archer.MicroLang.Tests.``UnitTestExecutor Happy Path``

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.FrameworkTypes
open Archer.MicroLang
open Microsoft.FSharp.Control

let private container = suite.Container ("TestLibrary", "UnitTestExecutor happy path")

let ``Test Cases`` = [
    container.Test("Should have the creating test as its parent", fun _ ->
        let executor = buildDummyExecutor None None
        
        executor.Parent
        |> expects.ToBe executor.Parent
    )
    
    container.Test ("Should return success if test action returns success", fun _ ->
        let test = buildDummyExecutor None None
        
        test
        |> getNoFrameworkInfoFromExecution
        |> test.Execute 
    )
    
    container.Test("Should raise all events in correct order", fun _ ->
        let executor = buildDummyExecutor None None
        
        let mutable cnt = 0
        let notRun = expects.GeneralNotRunFailure () |> TestFailure
        let mutable result = notRun
        
        let combineResult = combineResultIgnoring notRun
            
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestExecutionStarted _ ->
                let r =
                    cnt
                    |> expects.ToBe 0
                    |> withMessage "Start Execution out of order"
                
                cnt <- cnt + 1
                result <-
                    r |> combineResult result
            | TestSetupStarted _ ->
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
            | TestStartTearDown ->
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
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
]