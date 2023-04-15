module Archer.MicroLang.Tests.``UnitTestExecutor StartTest should``

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let private container = suite.Container ()

let ``be raised when the test is executed`` =
    container.Test (fun _ ->
        let container = suite.Container ("test", "container")
        let test = container.Test successfulTest
        let executor = test.GetExecutor ()
        
        let mutable result = expects.GeneralNotRunFailure () |> TestFailure
        
        executor.TestLifecycleEvent
        |> Event.add (fun args ->
            match args with
            | TestStart _ -> result <- TestSuccess
            | _ -> ()
        )
        
        executor
        |> getEmptyEnvironment
        |> executor.Execute
        |> ignore
        
        result
    )
    
let ``Test Cases`` = container.Tests