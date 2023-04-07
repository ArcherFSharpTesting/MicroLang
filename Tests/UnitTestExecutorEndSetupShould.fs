module Archer.MicroLang.Tests.``UnitTestExecutor EndSetup``

open Archer.MicroLang.Lang
open Archer.MicroLang.Types
open Archer.CoreTypes

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor EndSetup should")

let ``Test Cases`` = [
    container.Test ("be raised when the test is executed", fun () ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure
        executor.EndSetup.AddHandler (fun tst _ ->
            result <- tst |> expectsToBe executor.Parent
        )
        
        executor.Execute ()
        |> ignore
        
        result
    )
    
    container.Test ("carry the result of the StartSetup event", fun () ->
        let expectedFailure = "Failures abound" |> SetupFailure |> TestFailure
        let setupPart = SetupPart (fun () -> expectedFailure) |> Some
        let executor = buildDummyExecutor None setupPart
        
        let mutable result = notRunGeneralFailure
        
        executor.EndSetup.Add (fun args ->
            result <- args.TestResult
        )
        
        executor.Execute () |> ignore
        
        result
        |> expectsToBe expectedFailure
    )
    
    container.Test ("prevent the call of the test action if canceled", fun () ->
        let mutable result = TestSuccess
        
        let testAction () =
            result <- notRunValidationFailure
            TestSuccess
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.EndSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()  |> ignore
        
        result
    )
    
    container.Test ("should cause execution to return a CancelError if canceled", fun () ->
        let executor = buildDummyExecutor None None
        
        executor.EndSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()
        |> expectsToBe (TestFailure CancelFailure)
    )
    
    container.Test ("should carry result of setup action fails", fun () ->
        let expectedFailure = "This is an intended failure" |> SetupFailure |> TestFailure
        let setupAction =
            (fun () -> expectedFailure)
            |> SetupPart
            |> Some
            
        let mutable result = notRunGeneralFailure
        
        let executor = buildDummyExecutor None setupAction
        
        executor.EndSetup.Add (fun args ->
            result <-
                args.TestResult
                |> expectsToBe expectedFailure
        )
        
        executor.Execute () |> ignore
        
        result
    )
]