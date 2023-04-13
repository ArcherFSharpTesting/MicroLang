module Archer.MicroLang.Tests.``UnitTestExecutor StartTearDown``
    
open Archer.CoreTypes.InternalTypes
open Archer
open Archer.MicroLang

let private container = suite.Container ("TestLibrary", "UnitTestExecutor StartTearDown should")

let ``Test Cases`` = [
    container.Test ("be raised when the test is executed", fun _ ->
        let executor = buildDummyExecutor None None
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        executor.TestLifecycleEvent.Add (fun args ->
            match args with
            | TestStartTearDown -> result <- TestSuccess
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
]