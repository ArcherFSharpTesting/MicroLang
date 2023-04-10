module Archer.MicroLang.Tests.``UnitTestExecutor Happy Path``

open System
open Archer.MicroLang

let private container = suite.Container ("TestLibrary", "UnitTestExecutor happy path")

let ``Test Cases`` = [
    container.Test("Should have the creating test as its parent", fun _ ->
        let executor = buildDummyExecutor None None
        
        executor.Parent
        |> expectsToBe executor.Parent
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
        let mutable result = notRunGeneralFailure
        
        let combineResult = combineResultIgnoring notRunGeneralFailure
            
        executor.StartExecution.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 0 "Start Execution out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.StartSetup.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 1 "Start Setup out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.EndSetup.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 2 "End Setup out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.StartTest.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 3 "Start Test out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.EndTest.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 4 "End Test out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.StartTearDown.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 5 "Start Tear Down out of order"
                
            cnt <- cnt + 1
            result <- r |> combineResult result
        )
        
        executor.EndExecution.AddHandler (fun _ _ ->
            let r =
                cnt
                |> expectsToBeWithMessage 6 "End Execution out of order"
                
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