module Archer.MicroLang.Tests.``UnitTest should``

open Archer.CoreTypes.InternalTypes

open Archer
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ()

let ``have the test name`` =
    container.Test (fun _ ->
        let expectedName = "My Test Name"
        let test = UnitTest (ignoreString (), ignoreString (), expectedName, [], successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.TestName
        |> expects.ToBe expectedName
    )
    
let ``have the container name`` =
    container.Test (fun _ ->
        let expectedName = "My Container Name"
        let test = UnitTest (ignoreString(), expectedName, ignoreString (), [], successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.ContainerName
        |> expects.ToBe expectedName
    )
    
let ``have the location`` =
    container.Test (fun _ ->
        let location = {
            FilePath = ignoreString ()
            FileName = ignoreString ()
            LineNumber = ignoreInt () 
        }
        let test = UnitTest (ignoreString (), ignoreString(), ignoreString (), [], successfulTest, EmptyPart, location) :> ITest
        
        test.Location
        |> expects.ToBe location
    )
    
let ``have tags`` =
    container.Test (fun _ ->
        let tags = [Category "My Test"]
        let test = UnitTest (ignoreString (), ignoreString (), ignoreString (), tags, successfulTest, EmptyPart, ignoreLocation ()) :> ITest
        
        test.Tags
        |> expects.ToBe tags
    )
    
let ``have well formed string representation`` =
    container.Test (fun _ ->
        let test = UnitTest ("Container Path", "Container Name", "Test Name", [], successfulTest, EmptyPart, ignoreLocation ())
        
        test.ToString ()
        |> expects.ToBe "Container Path <> Container Name <> Test Name"
    )
    
let ``Test Cases`` = [
    ``have the test name``
    ``have the container name``
    ``have the location``
    ``have tags``
    ``have well formed string representation``
]