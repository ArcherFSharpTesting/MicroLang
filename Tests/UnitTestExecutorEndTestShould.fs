module Archer.MicroLang.Tests.``UnitTestExecutor EndTest should``

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` = 
    container.Test (fun _ ->
        let container = suite.Container ("some", "container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure 
        
        executor.TestLifecycleEvent.Add (fun args ->
            match args with
            | TestEnd _ -> result <- TestSuccess
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = container.Tests