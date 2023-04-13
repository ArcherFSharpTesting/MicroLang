namespace Archer.MicroLang.Types

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer
open Archer.CoreTypes.InternalTypes

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
        
    let raiseTestStart capture env cancelEventArgs =
        testLifecycleEvent.Trigger (parent, TestStart cancelEventArgs)
        if cancelEventArgs |> shouldContinue then
            test env |> capture
            cancelEventArgs
        else
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
            | TestFailure a, TestFailure b ->
                result <- CombinationFailure (a, b) |> TestFailure
            | TestFailure _ as failure, _
            | _, (TestFailure _ as failure) -> result <- failure
            | Ignored _ as ing, _
            | _, (Ignored _ as ing) -> result <- ing
            | _ -> ()
            
        CancelEventArgs ()
        |> raiseStartExecution
        |> raiseStartSetup writeResult
        |> raiseEndSetup result
        |> raiseTestStart writeResult env
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
    member _.Test(testName: string, action: FrameworkEnvironment -> TestResult, part: TestPart, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        UnitTest (containerPath, containerName, testName, [], action, part, buildLocation fullPath lineNumber) :> ITest
    
    member this.Test (testName: string, action: FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test(testName, action, EmptyPart, fullPath, lineNumber)
    
type TestContainerBuilder () =
    member _.Container (containerPath: string, containerName: string) =
        TestBuilder (containerPath, containerName)