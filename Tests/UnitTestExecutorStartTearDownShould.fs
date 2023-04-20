module Archer.MicroLang.Tests.``UnitTestExecutor StartTearDown should``
    
open Archer.CoreTypes.InternalTypes
open Archer
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` =
    container.Test (
        SetupPart setupExecutor,
        
        fun executor _ ->
            let mutable result = newFailure.With.TestExecutionShouldNotRunFailure () |> TestFailure
            
            executor.TestLifecycleEvent.Add (fun args ->
                match args with
                | TestStartTeardown -> result <- TestSuccess
                | _ -> ()
            )
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> ignore
            
            result
    )

let ``Test Cases`` = container.Tests