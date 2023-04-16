module Archer.MicroLang.Tests.``UnitTestExecutor EndExecution should``
    
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` =
    container.Test (
        SetupPart setupExecutor,
        
        fun executor _ ->
            let mutable result = newFailure.With.GeneralNotRunFailure () |> TestFailure
            
            executor.TestLifecycleEvent
            |> Event.add (fun args ->
                match args with
                | TestEndExecution _ ->
                    result <- TestSuccess
                | _ -> ()
            )
            
            executor
            |> getEmptyEnvironment
            |> executor.Execute
            |> ignore
            
            result
    )

let ``Test Cases`` = container.Tests