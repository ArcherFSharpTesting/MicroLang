module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup``

open Archer.MicroLang
open Archer.MicroLang.Types
open Archer

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor EndSetup should")

let ``Test Cases`` = [
    container.Test ("be raised when the test is executed", fun _ ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure
        executor.EndSetup.AddHandler (fun tst _ ->
            result <- tst |> expectsToBe executor.Parent
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
    
    container.Test ("carry the result of the StartSetup event", fun _ ->
        let expectedFailure = "Failures abound" |> SetupFailure |> TestFailure
        let setupPart = SetupPart (fun _ -> expectedFailure) |> Some
        let executor = buildDummyExecutor None setupPart
        
        let mutable result = notRunGeneralFailure
        
        executor.EndSetup.Add (fun args ->
            result <- args.TestResult
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
        |> expectsToBe expectedFailure
    )
    
    container.Test ("prevent the call of the test action if canceled", fun _ ->
        let mutable result = TestSuccess
        
        let testAction _ =
            result <- notRunValidationFailure
            TestSuccess
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.EndSetup.Add (fun args ->
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
        
        executor.EndSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> expectsToBe (TestFailure CancelFailure)
    )
    
    container.Test ("should carry result of setup action fails", fun _ ->
        let expectedFailure = "This is an intended failure" |> SetupFailure |> TestFailure
        let setupAction =
            (fun _ -> expectedFailure)
            |> SetupPart
            |> Some
            
        let mutable result = notRunGeneralFailure
        
        let executor = buildDummyExecutor None setupAction
        
        executor.EndSetup.Add (fun args ->
            result <-
                args.TestResult
                |> expectsToBe expectedFailure
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
]