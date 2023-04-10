module Archer.MicroLang.Tests.``UnitTest Base Case``

open Archer.CoreTypes.InternalTypes

open Archer
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTest should")

let ``Test Cases`` = [
    container.Test ("have the test name", fun _ ->
        let expectedName = "My Test Name"
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString (), expectedName, ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.TestName
        |> expectsToBe expectedName
    )

    container.Test ("have the container name", fun _ ->
        let expectedName = "My Container Name"
        let test = UnitTest (ignorePath (), ignoreString(), expectedName, ignoreString (), ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.ContainerName
        |> expectsToBe expectedName
    )
    
    container.Test ("have the line number", fun _ ->
        let expectedLineNumber = 66
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString(), ignoreString (), expectedLineNumber, [], successfulTest, EmptyPart) :> ITest
        
        test.LineNumber
        |> expectsToBe expectedLineNumber
    )
    
    container.Test ("have tags", fun _ ->
        let tags = [Category "My Test"]
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString (), ignoreString (), ignoreInt (), tags, successfulTest, EmptyPart) :> ITest
        
        test.Tags
        |> expectsToBe tags
    )
    
    container.Test ("have well formed string representation", fun _ ->
        let test = UnitTest (ignorePath (), "Container Path", "Container Name", "Test Name", 47, [], successfulTest, EmptyPart)
        
        test.ToString ()
        |> expectsToBe "Container Path <> Container Name <> Test Name"
    )
]