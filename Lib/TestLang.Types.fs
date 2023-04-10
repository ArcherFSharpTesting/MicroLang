namespace Archer.MicroLang.Types

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer
open Archer.CoreTypes.InternalTypes

module TypeSupport =
    let success () = TestSuccess
    let maybeTriggerCancel sender (event: Event<'a, 'b :> CancelEventArgs>) (getCancel: unit -> 'b) previousResult =
        let cancelArgs = getCancel ()
        event.Trigger (sender, cancelArgs)

        match cancelArgs.Cancel with
        | true -> CancelFailure |> TestFailure
        | _ -> previousResult

    let trigger sender (event: Event<'a, TestEventArgs>) previousResult =
        match previousResult with
        | TestFailure CancelFailure -> previousResult
        | _ ->
            let args = TestEventArgs previousResult
            event.Trigger (sender, args)
            previousResult

    let wrapEvent event sender previousResult = 
        trigger sender event previousResult

    let wrapCancel event sender previousResult =
        maybeTriggerCancel sender event CancelEventArgs previousResult

    let wrapCancelResult event sender previousResult =
        maybeTriggerCancel sender event (fun () -> TestCancelEventArgsWithResults previousResult) previousResult

    let joinCancelEventResult event action sender previousResult =
        let result = wrapCancelResult event sender previousResult
        match result with
        | TestFailure _ -> result
        | _ -> action ()

    let joinCancelEvent event action sender previousResult =
        let result = wrapCancel event sender previousResult
        match result with
        | TestFailure _ -> result
        | _ -> action ()
        
    let joinEvent (event: Event<TestDelegate, EventArgs>) action sender previousResult =
        event.Trigger (sender, EventArgs.Empty)
        
        let result = action ()
        
        match previousResult, result with
        | TestFailure a, TestFailure b -> CombinationFailure (a, b) |> TestFailure
        | TestFailure _, _ -> previousResult
        | _, TestFailure _ -> result
        | _ -> TestSuccess
        
open TypeSupport
        
type UnitTestExecutor (parent: ITest, setup: unit -> TestResult, test: FrameworkEnvironment -> TestResult, tearDown: unit -> TestResult) =
    let startExecution = Event<CancelDelegate, CancelEventArgs> ()
    let startSetup = Event<CancelDelegate, CancelEventArgs> ()
    let endSetup = Event<CancelTestDelegate, TestCancelEventArgsWithResults> ()
    let startTest = Event<CancelDelegate, CancelEventArgs> ()
    let endTest = Event<TestResultDelegate, TestEventArgs> ()
    let startTearDown = Event<TestDelegate, EventArgs> ()
    let endExecution = Event<TestResultDelegate, TestEventArgs> ()
    [<CLIEvent>]
    member _.StartExecution = startExecution.Publish
    [<CLIEvent>]
    member _.StartSetup = startSetup.Publish
    [<CLIEvent>]
    member _.EndSetup = endSetup.Publish
    [<CLIEvent>]
    member _.StartTest = startTest.Publish
    [<CLIEvent>]
    member _.EndTest = endTest.Publish
    [<CLIEvent>]
    member _.StartTearDown = startTearDown.Publish
    [<CLIEvent>]
    member _.EndExecution = endExecution.Publish
    
    member _.Parent with get () = parent
    
    member _.Execute env =
        TestSuccess
        |> wrapCancel startExecution parent
        |> joinCancelEvent startSetup setup parent
        |> wrapCancelResult endSetup parent
        |> joinCancelEvent startTest (fun () -> test env) parent
        |> wrapEvent endTest parent
        |> joinEvent startTearDown tearDown parent
        |> wrapEvent endExecution parent
        
    override this.ToString () =
        $"%s{this.Parent.ToString ()}.Executor"
    
    interface ITestExecutor with
        [<CLIEvent>]
        member this.StartExecution = this.StartExecution
        [<CLIEvent>]
        member this.StartSetup = this.StartSetup
        [<CLIEvent>]
        member this.EndSetup = this.EndSetup
        [<CLIEvent>]
        member this.StartTest = this.StartTest
        [<CLIEvent>]
        member this.EndTest = this.EndTest
        [<CLIEvent>]
        member this.StartTearDown = this.StartTearDown
        [<CLIEvent>]
        member this.EndExecution = this.EndExecution
        member _.Parent with get () = parent
        member this.Execute env = this.Execute env
            
type TestPart =
    | EmptyPart
    | SetupPart of (unit -> TestResult)
    | TearDownPart of (unit -> TestResult)
    | Both of setup: (unit -> TestResult) * tearDown: (unit -> TestResult)
            
type UnitTest (filePath: string, containerPath: string, containerName: string, testName: string, lineNumber: int, tags: TestTag seq, test: FrameworkEnvironment -> TestResult, testParts: TestPart) =
    let setup, tearDown =
        match testParts with
        | EmptyPart -> success, success
        | SetupPart setup -> setup, success
        | TearDownPart tearDown -> success, tearDown
        | Both (setup, tearDown) -> setup, tearDown
        
    let fileName =
        if String.IsNullOrEmpty filePath then ""
        else
            System.IO.Path.GetFileName filePath

    override this.ToString () =
        let test = this :> ITest
        [
            test.ContainerPath
            test.ContainerName
            test.TestName
        ]
        |> List.filter (String.IsNullOrEmpty >> not)
        |> fun items -> String.Join (" <> ", items)
        
    member _.ContainerFullName = containerPath
    member _.ContainerName = containerName
    member _.LineNumber = lineNumber
    member _.Tags = tags
    member _.TestName = testName
    
    member this.GetExecutor() =
        UnitTestExecutor (this, setup, test, tearDown)
        :> ITestExecutor
        
    interface ITest with
        member _.ContainerPath with get () = containerPath
        member _.ContainerName with get () = containerName
        member _.LineNumber with get () = lineNumber
        member _.Tags with get () = tags
        member _.TestName with get () = testName
        member _.FileName with get () = fileName
        member _.FilePath with get () = filePath
        
        member this.GetExecutor() = this.GetExecutor ()
            
type TestBuilder (containerPath: string, containerName: string) =
    member _.Test(testName: string, action: FrameworkEnvironment -> TestResult, part: TestPart, [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        UnitTest (path, containerPath , containerName, testName, lineNumber, [], action, part) :> ITest
    
    member this.Test (testName: string, action: FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test(testName, action, EmptyPart, path, lineNumber)
    
type TestContainerBuilder () =
    member _.Container (containerPath: string, containerName: string) =
        TestBuilder (containerPath, containerName)