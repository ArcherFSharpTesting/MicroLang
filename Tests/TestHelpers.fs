[<Microsoft.FSharp.Core.AutoOpen>]
module Archer.MicroLang.Tests.TestHelpers

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang

let getEmptyEnvironment (test: ITestExecutor) =
    {
        RunnerName = "No Framework"
        RunnerVersion = Version "0.0.0.0"
        TestInfo = test.Parent 
    }
    
let ignoreLocation () = {
    FilePath = ignoreString ()
    FileName = ignoreString ()
    LineNumber = ignoreInt ()
}

let rec getContainerUnderTest _ =
    suite.Container ("Under Test", "Container")
    
let setupExecutor _ =
    let container = getContainerUnderTest ()
    let test = container.Test (ignoreString (), successfulTest)
    test.GetExecutor () |> Ok
    
let setupExecutorFromResult _ =
    let testFromResult (testResult: TestResult) =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), fun _ -> testResult)
        test.GetExecutor ()
    
    testFromResult |> Ok
    
let setupExecutorFromTestAction _ =
    let testFromAction testAction =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), testAction)
        test.GetExecutor ()
    
    testFromAction |> Ok
    
let setupExecutorFromSetupResult _ =
    let testFromSetupAction setupResult =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), SetupPart (fun _ -> setupResult), successfulEnvironmentTest)
        test.GetExecutor ()
        
    testFromSetupAction |> Ok
    
let setupExecutorFromSetupAction _ =
    let setupTest setupAction =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), SetupPart setupAction, successfulEnvironmentTest)
        test.GetExecutor ()
    
    setupTest |> Ok
    
let setupExecutorFromTestActionAndSetupResult _ =
    let testBuilder setupResult testAction =
        let container = getContainerUnderTest ()
        let test = container.Test (ignoreString (), SetupPart (fun _ -> setupResult), testAction)
        test.GetExecutor ()
    
    testBuilder |> Ok