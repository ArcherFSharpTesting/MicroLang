module Archer.MicroLang.Tests.``UnitTest Base Case``

open Archer.CoreTypes.InternalTypes

open Archer
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ("TestingLibrary", "UnitTest should")

let ``Test Cases`` = [
    container.Test ("have the test name", fun _ ->
        let expectedName = "My Test Name"
        let test = UnitTest (ignoreString (), ignoreString (), expectedName, [], successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.TestName
        |> expects.ToBe expectedName
    )

    container.Test ("have the container name", fun _ ->
        let expectedName = "My Container Name"
        let test = UnitTest (ignoreString(), expectedName, ignoreString (), [], successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.ContainerName
        |> expects.ToBe expectedName
    )
    
    container.Test ("have the location", fun _ ->
        let expectedLineNumber = 66
        let location = {
            FilePath = ignoreString ()
            FileName = ignoreString ()
            LineNumber = ignoreInt () 
        }
        let test = UnitTest (ignoreString (), ignoreString(), ignoreString (), [], successfulTest, EmptyPart, location) :> ITest
        
        test.Location
        |> expects.ToBe location
    )
    
    container.Test ("have tags", fun _ ->
        let tags = [Category "My Test"]
        let test = UnitTest (ignoreString (), ignoreString (), ignoreString (), tags, successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.Tags
        |> expects.ToBe tags
    )
    
    container.Test ("have well formed string representation", fun _ ->
        let test = UnitTest ("Container Path", "Container Name", "Test Name", [], successfulTest, EmptyPart, ignoreLocation ())
        
        test.ToString ()
        |> expects.ToBe "Container Path <> Container Name <> Test Name"
    )
]