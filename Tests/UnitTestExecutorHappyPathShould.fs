module Archer.MicroLang.Tests.``UnitTestExecutor happy path``

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang
open Microsoft.FSharp.Control

let private container = suite.Container ()

let ``Should have the creating test as its parent`` =
    container.Test(
        SetupPart (fun _ ->
            let container = suite.Container (ignoreString (), ignoreString ())
            let test = container.Test successfulTest
            
            test |> Ok 
        ),
        
        fun test _ ->
            let executor = test.GetExecutor ()
            
            executor.Parent
            |> expects.ToBe test
    )
    
let ``Should return success if test action returns success`` =
    container.Test (
        SetupPart setupExecutor,
        
        fun executor _ ->
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> expects.ToBe (TestSuccess |> TestExecutionResult)
    )
    
let ``Should raise all events in correct order`` =
    container.Test(
        SetupPart setupExecutor,
        
        fun executor _ ->
            let mutable cnt = 0
            let notRun = newFailure.With.GeneralNotRunFailure () |> TestFailure
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