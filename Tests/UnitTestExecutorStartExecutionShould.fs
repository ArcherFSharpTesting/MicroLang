module Archer.MicroLang.Tests.``UnitTestExecutor StartExecution``

open Archer.MicroLang.Types
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ("TestingLibrary", "UnitTestExecutor StartExecution should")

let ``Test Cases`` = [
    container.Test ("be raised when test is executed", fun () ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure
        executor.StartExecution.AddHandler (CancelDelegate (fun tst _ ->
                result <- tst |> expectsToBe executor.Parent
            )
        )
        
        executor.Execute () |> ignore
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
        
        executor.StartExecution.Add (fun args ->
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
        
        executor.StartExecution.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()  |> ignore
        
        result
    )
    
    container.Test ("should cause execution to return a CancelError if canceled", fun () ->
        let executor = buildDummyExecutor None None
        
        executor.StartExecution.Add (fun args ->
            args.Cancel <- true
        )
        
        executor.Execute ()
        |> expectsToBe (TestFailure CancelFailure)
    )
]