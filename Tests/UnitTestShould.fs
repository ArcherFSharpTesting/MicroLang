module Archer.MicroLang.Tests.``UnitTest should``

open Archer.CoreTypes.InternalTypes

open Archer
open Archer.MicroLang
open Archer.MicroLang.Types

let private container = suite.Container ()

let ``have the test name`` =
    container.Test (fun _ ->
        let expectedName = "My Test Name"
        let test = UnitTest (ignoreString (), ignoreString (), expectedName, [], successfulEnvironmentTest, successfulUnitSetup, successfulTeardown, ignoreLocation ()) :> ITest
        
        test.TestName
        |> expects.ToBe expectedName
    )
    
let ``have the container name`` =
    container.Test (fun _ ->
        let expectedName = "My Container Name"
        let test = UnitTest (ignoreString(), expectedName, ignoreString (), [], successfulEnvironmentTest, successfulUnitSetup, successfulTeardown, ignoreLocation ()) :> ITest
        
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
        let test = UnitTest (ignoreString (), ignoreString(), ignoreString (), [], successfulEnvironmentTest, successfulUnitSetup, successfulTeardown, location) :> ITest
        
        test.Location
        |> expects.ToBe location
    )
    
let ``have tags`` =
    container.Test (fun _ ->
        let tags = [Category "My Test"]
        let test = UnitTest (ignoreString (), ignoreString (), ignoreString (), tags, successfulEnvironmentTest, successfulUnitSetup, successfulTeardown, ignoreLocation ()) :> ITest
        
        test.Tags
        |> expects.ToBe tags
    )
    
let ``have well formed string representation`` =
    container.Test (fun _ ->
        let test = UnitTest ("Container Path", "Container Name", "Test Name", [], successfulEnvironmentTest, (fun _ -> Ok ()), (fun _ _ -> Ok ()), ignoreLocation ())
        
        test.ToString ()
        |> expects.ToBe "Container Path <> Container Name <> Test Name"
    )
    
let ``Test Cases`` = container.Tests