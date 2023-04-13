module Archer.MicroLang.Tests.``UnitTestExecutor StartTearDown should``
    
open Archer.CoreTypes.InternalTypes
open Archer
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` =
    container.Test (fun _ ->
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

let ``Test Cases`` = container.Tests