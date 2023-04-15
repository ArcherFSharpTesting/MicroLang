[<Microsoft.FSharp.Core.AutoOpen>]
module Archer.MicroLang.Tests.TestHelpers

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let getEmptyEnvironment (test: ITestExecutor) =
    {
        FrameworkName = "No Framework"
        FrameworkVersion = Version "0.0.0.0"
        TestInfo = test.Parent 
    }
    
let ignoreLocation () = {
    FilePath = ignoreString ()
    FileName = ignoreString ()
    LineNumber = ignoreInt ()
}

let rec getContainerUnderTest _ =
    suite.Container ("Under Test", "Container")
    
let setupTest _ =
    let container = getContainerUnderTest ()
    let test = container.Test (ignoreString (), successfulTest)
    test.GetExecutor () |> Ok
    
let setupTestFromResult _ =
    let testFromResult (testResult: TestResult) =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), fun _ -> testResult)
        test.GetExecutor ()
    
    testFromResult |> Ok
    
let setupTestFromTestAction _ =
    let testFromAction testAction =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), testAction)
        test.GetExecutor ()
    
    testFromAction |> Ok
    
let setupTestFromSetupResult _ =
    let testFromSetupAction setupResult =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), Setup (fun _ -> setupResult), successfulEnvironmentTest)
        test.GetExecutor ()
        
    testFromSetupAction |> Ok
    
let setupTestFromTestActionAndSetupResult _ =
    let testBuilder setupResult testAction =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), Setup (fun _ -> setupResult), testAction)
        test.GetExecutor ()
    
    testBuilder |> Ok