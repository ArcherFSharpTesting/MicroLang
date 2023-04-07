module Archer.MicroLang.Tests.``UnitTestExecutor EndTest``

open Archer.MicroLang.Lang

let private container = suite.Container ("TestLibrary", "UnitTestExecutor EndTest should")

let ``Test Cases`` = [
    container.Test ("be raised when the test is executed", fun () ->
        let executor = buildDummyExecutor None None
        
        let mutable result = notRunGeneralFailure 
        executor.EndTest.AddHandler (fun tst _ ->
            result <- tst |> expectsToBe executor.Parent
        )
        
        executor.Execute ()
        |> ignore
        
        result
    )
]