module Archer.MicroLang.Tests.``UnitTest Base Case``

open Archer.CoreTypes.InternalTypes

open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTest should")

let ``Test Cases`` = [
    container.Test ("have the test name", fun () ->
        let expectedName = "My Test Name"
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString (), expectedName, ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.TestName
        |> expectsToBe expectedName
    )

    container.Test ("have the container name", fun () ->
        let expectedName = "My Container Name"
        let test = UnitTest (ignorePath (), ignoreString(), expectedName, ignoreString (), ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.ContainerName
        |> expectsToBe expectedName
    )
    
    container.Test ("have the container fullname", fun () ->
        let expectedName = "My Container Full Name"
        let test = UnitTest (ignorePath (), expectedName, ignoreString (), ignoreString (), ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.ContainerFullName
        |> expectsToBe expectedName
    )
    
    container.Test ("have the test fullname", fun () ->
        let testName = "My Test Name"
        let containerFullName = "My Container Full Name"
        let expectedName = $"%s{containerFullName} <> %s{testName}"
        let test = UnitTest (ignorePath (), containerFullName, ignoreString (), testName, ignoreInt (), [], successfulTest, EmptyPart) :> ITest
        
        test.TestFullName
        |> expectsToBe expectedName
    )
    
    container.Test ("have the line number", fun () ->
        let expectedLineNumber = 66
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString(), ignoreString (), expectedLineNumber, [], successfulTest, EmptyPart) :> ITest
        
        test.LineNumber
        |> expectsToBe expectedLineNumber
    )
    
    container.Test ("have tags", fun () ->
        let tags = [Category "My Test"]
        let test = UnitTest (ignorePath (), ignoreString (), ignoreString (), ignoreString (), ignoreInt (), tags, successfulTest, EmptyPart) :> ITest
        
        test.Tags
        |> expectsToBe tags
    )
    
    container.Test ("have well formed string representation", fun () ->
        let test = UnitTest (ignorePath (), "Container Full Path", ignoreString (), "Test Name", 47, [], successfulTest, EmptyPart)
        
        test.ToString ()
        |> expectsToBe "Container Full Path <> Test Name"
    )
]