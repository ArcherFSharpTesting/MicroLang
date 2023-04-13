namespace Archer.MicroLang.Types

open System
open System.ComponentModel
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer
open Archer.CoreTypes.InternalTypes
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

module TypeSupport =
    let success () = TestSuccess
    
    let shouldContinue (cancelEventArgs: CancelEventArgs) =
        cancelEventArgs.Cancel |> not
        
    let testCancelFailure = CancelFailure |> TestFailure
    
    let buildLocation (fullPath: string) lineNumber =
        let fileInfo = System.IO.FileInfo fullPath
        let path = fileInfo.Directory.FullName
        let fileName = fileInfo.Name
        
        {
            FilePath = path
            FileName = fileName
            LineNumber = lineNumber 
        }
    
open TypeSupport
        
type UnitTestExecutor (parent: ITest, setup: unit -> TestResult, test: FrameworkEnvironment -> TestResult, tearDown: unit -> TestResult) as this=
    let testLifecycleEvent = Event<TestExecutionDelegate, TestEventLifecycle> ()
    
    let raiseStartExecution cancelEventArgs =
        testLifecycleEvent.Trigger (parent, TestExecutionStarted cancelEventArgs)
        cancelEventArgs
        
    let raiseStartSetup capture cancelEventArgs =
        testLifecycleEvent.Trigger (parent, TestSetupStarted cancelEventArgs)
        if cancelEventArgs |> shouldContinue then
            setup () |> capture
            cancelEventArgs
        else
            testCancelFailure |> capture
            cancelEventArgs
        
    let raiseEndSetup testResult cancelEventArgs =
        testLifecycleEvent.Trigger (parent, TestEndSetup (testResult, cancelEventArgs))
        cancelEventArgs
        
    let raiseTestStart lastResult capture env cancelEventArgs =
        testLifecycleEvent.Trigger (parent, TestStart cancelEventArgs)
        
        if cancelEventArgs |> shouldContinue && lastResult = TestSuccess then
            test env |> capture
        elif lastResult = TestSuccess then
            testCancelFailure |> capture
        
        cancelEventArgs
        
        
    let raiseTestEnd result arg =
        testLifecycleEvent.Trigger (parent, TestEnd result)
        arg
        
    let raiseStartTearDown capture arg =
        testLifecycleEvent.Trigger (parent, TestStartTearDown)
        tearDown () |> capture
        arg
        
    let raiseEndExecution result arg =
        testLifecycleEvent.Trigger (parent, TestEndExecution result)
        arg
        
    member _.Parent with get () = parent
    
    member _.Execute env =
        let mutable result = TestSuccess
        
        let writeResult value =
            match result, value with
            | TestFailure _ as failure, _
            | _, (TestFailure _ as failure) -> result <- failure
            | TestIgnored _ as ing, _
            | _, (TestIgnored _ as ing) -> result <- ing
            | _ -> result <- TestSuccess
            
        CancelEventArgs ()
        |> raiseStartExecution
        |> raiseStartSetup writeResult
        |> raiseEndSetup result
        |> raiseTestStart result writeResult env
        |> raiseTestEnd result
        |> raiseStartTearDown writeResult
        |> raiseEndExecution result
        |> ignore
        
        result
        
    override this.ToString () =
        $"%s{this.Parent.ToString ()}.Executor"
    
    interface ITestExecutor with
        member _.Parent with get () = parent
        member this.Execute env = this.Execute env
        [<CLIEvent>]
        member this.TestLifecycleEvent = testLifecycleEvent.Publish
            
type TestPart =
    | EmptyPart
    | SetupPart of (unit -> TestResult)
    | TearDownPart of (unit -> TestResult)
    | Both of setup: (unit -> TestResult) * tearDown: (unit -> TestResult)
            
type UnitTest (containerPath: string, containerName: string, testName: string, tags: TestTag seq, test: FrameworkEnvironment -> TestResult, testParts: TestPart, location: CodeLocation) =
    let setup, tearDown =
        match testParts with
        | EmptyPart -> success, success
        | SetupPart setup -> setup, success
        | TearDownPart tearDown -> success, tearDown
        | Both (setup, tearDown) -> setup, tearDown

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
    member _.Tags = tags
    member _.TestName = testName
    member _.Location = location
    
    member this.GetExecutor() =
        UnitTestExecutor (this, setup, test, tearDown)
        :> ITestExecutor
        
    interface ITest with
        member _.ContainerPath with get () = containerPath
        member _.ContainerName with get () = containerName
        member _.Tags with get () = tags
        member _.TestName with get () = testName
        
        member this.GetExecutor() = this.GetExecutor ()
        member this.Location with get () = location
            
type TestBuilder (containerPath: string, containerName: string) =
    let mutable tests : ITest list = []
    member _.Test(action: FrameworkEnvironment -> TestResult, part: TestPart, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let test = UnitTest (containerPath, containerName, testName, [], action, part, buildLocation fullPath lineNumber) :> ITest
        tests <- test::tests
        test
    
    member this.Test (action: FrameworkEnvironment -> TestResult, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test(action, EmptyPart, testName, fullPath, lineNumber)
        
    member this.Test( testName: string, action: FrameworkEnvironment -> TestResult, part: TestPart, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (action, part, testName, fullPath, lineNumber)

    member this.Test (testName: string, action: FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (action, testName, fullPath, lineNumber)
        
    member _.Tests with get () = tests

    
type TestContainerBuilder () =
    member _.Container ([<Optional; DefaultParameterValue("")>]containerPath: string, [<Optional; DefaultParameterValue("")>]containerName: string) =
        let containerName, containerPath =
            if containerName |> String.IsNullOrWhiteSpace || containerPath |> String.IsNullOrWhiteSpace then
                let trace = StackTrace ()
                let method = trace.GetFrame(1).GetMethod ()
                let containerName =
                    if containerName |> String.IsNullOrWhiteSpace then method.ReflectedType.Name
                    else containerName
                let containerPath =
                    if containerPath |> String.IsNullOrWhiteSpace then method.ReflectedType.Namespace |> fun s -> s.Split ([|"$"|], StringSplitOptions.RemoveEmptyEntries) |> Array.last
                    else containerPath
                    
                containerName, containerPath
            else
                containerName, containerPath
        
        TestBuilder (containerPath, containerName)