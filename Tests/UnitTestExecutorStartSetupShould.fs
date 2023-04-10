module Archer.MicroLang.Tests.``UnitTestExecutor StartSetup``

open Archer.MicroLang
open Archer
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor StartSetup should")

let ``Test Cases`` = [
    container.Test ("be raised when test is executed", fun _ ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure
        executor.StartSetup.AddHandler (fun tst _ ->
            result <- tst |> expectsToBe executor.Parent
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test setup if canceled", fun _ ->
        let mutable result = TestSuccess
        
        let setupPart =
            SetupPart (fun () ->
                result <- notRunValidationFailure
                TestSuccess
            )
            |> Some
            
        let executor = buildDummyExecutor None setupPart
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
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
            result <- notRunValidationFailure
            TestSuccess
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
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
            result <- notRunValidationFailure
            
            "some setup failure"
            |> SetupFailure
            |> TestFailure
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("should cause execution to return a CancelError if canceled", fun _ ->
        let executor = buildDummyExecutor None None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> expectsToBe (TestFailure CancelFailure)
    )
]