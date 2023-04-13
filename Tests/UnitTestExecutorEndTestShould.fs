module Archer.MicroLang.Tests.``UnitTestExecutor EndTest``

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ("TestLibrary", "UnitTestExecutor EndTest should")

let ``Test Cases`` = [
    container.Test ("be raised when the test is executed", fun _ ->
        let executor = buildDummyExecutor None None
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure 
        
        executor.TestLifecycleEvent.Add (fun args ->
            match args with
            | TestEnd _ -> result <- TestSuccess
            | _ -> ()
        )
        
        executor
        |> getNoFrameworkInfoFromExecution
        |> executor.Execute
        |> ignore
        
        result
    )
]