module Archer.MicroLang.Tests.``UnitTestExecutor EndTest should``

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` = 
    container.Test (fun _ ->
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
    
let ``Test Cases`` = container.Tests