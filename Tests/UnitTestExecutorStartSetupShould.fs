module Archer.MicroLang.Tests.``UnitTestExecutor StartSetup``

open Archer.MicroLang.Lang
open Archer.CoreTypes
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor StartSetup should")

let ``Test Cases`` = [
    container.Test ("be raised when test is executed", fun () ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure
        executor.StartSetup.AddHandler (fun tst _ ->
            result <- tst |> expectsToBe executor.Parent
        )
        
        executor.Execute ()
        |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test setup if canceled", fun () ->
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
        
        executor.Execute ()  |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test action if canceled", fun () ->
        let mutable result = TestSuccess
        
        let testAction () =
            result <- notRunValidationFailure
            TestSuccess
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()  |> ignore
        
        result
    )
    
    container.Test ("prevent the call of the test action if failed", fun () ->
        let mutable result = TestSuccess
        
        let testAction () =
            result <- notRunValidationFailure
            
            "some setup failure"
            |> SetupFailure
            |> TestFailure
            
        let executor = buildDummyExecutor (Some testAction) None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()  |> ignore
        
        result
    )
    
    container.Test ("should cause execution to return a CancelError if canceled", fun () ->
        let executor = buildDummyExecutor None None
        
        executor.StartSetup.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()
        |> expectsToBe (TestFailure CancelFailure)
    )
]