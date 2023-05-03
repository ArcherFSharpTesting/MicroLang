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
open Archer.MicroLang

module TypeSupport =
    let successfulTest _ _ = TestSuccess
    
    let successfulSetup () = Ok () 
    let successfulTeardown _ _ = Ok ()
    
    let shouldContinue (cancelEventArgs: CancelEventArgs) =
        cancelEventArgs.Cancel |> not
        
    let testCancelFailure = GeneralCancelFailure |> GeneralExecutionFailure
    
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

type ExecutionAccumulator<'a> = {
    SetupResult: Result<'a, SetupTeardownFailure> option
    TestResult: TestResult option
    TeardownResult: Result<unit, SetupTeardownFailure> option
}

type UnitTestExecutor<'a> (parent: ITest, setup: unit -> Result<'a, SetupTeardownFailure>, test: 'a -> FrameworkEnvironment -> TestResult, tearDown: Result<'a, SetupTeardownFailure> -> TestResult option -> Result<unit, SetupTeardownFailure>) =
    let testLifecycleEvent = Event<TestExecutionDelegate, TestEventLifecycle> ()
    
    let startExecution (cancelEventArgs: CancelEventArgs) (acc: ExecutionAccumulator<'a>) =
        testLifecycleEvent.Trigger (parent, TestStartExecution cancelEventArgs)
        acc
        
    let startSetup (cancelEventArgs: CancelEventArgs) (acc: ExecutionAccumulator<'a>) =
        let continueOn = cancelEventArgs.Cancel |> not
        if continueOn then
            testLifecycleEvent.Trigger (parent, TestStartSetup cancelEventArgs)
            if cancelEventArgs.Cancel |> not then
                let result = setup ()
                { acc with SetupResult = result |> Some }
            else
                acc
        else
            acc
            
    let endSetup (cancelEventArgs: CancelEventArgs) (acc: ExecutionAccumulator<'a>) =
        match acc.SetupResult with
        | Some (Ok _) -> testLifecycleEvent.Trigger (parent, TestEndSetup (SetupSuccess, cancelEventArgs))
        | Some (Error errorValue) -> testLifecycleEvent.Trigger (parent, TestEndSetup (errorValue |> SetupFailure, cancelEventArgs))
        | _ -> ()
        
        acc
        
    let startTest (cancelEventArgs: CancelEventArgs) environment (acc: ExecutionAccumulator<'a>) =
        match cancelEventArgs.Cancel, acc.SetupResult with
        | true, _ -> acc
        | _, Some (Ok v) ->
            testLifecycleEvent.Trigger (parent, TestStart cancelEventArgs)
            if cancelEventArgs.Cancel then
                acc
            else
                let result = test v environment
                { acc with TestResult = result |> Some }
        | _ -> acc
        
    let endTest (acc: ExecutionAccumulator<'a>) =
        match acc.TestResult with
        | Some result ->
            testLifecycleEvent.Trigger (parent, TestEnd result)
            acc
        | _ -> acc
        
    let startTearDown (cancelEventArgs: CancelEventArgs) (acc: ExecutionAccumulator<'a>) =
        match acc.SetupResult, cancelEventArgs.Cancel with
        | Some _, _
        | _, false ->
            testLifecycleEvent.Trigger (parent, TestStartTeardown)
            match acc.SetupResult, acc.TestResult with
            | Some setupResult, testResult ->
                let result = tearDown setupResult testResult
                { acc with TeardownResult =  Some result }
            | _ -> acc
        | _, true ->
            acc
            
    let endExecution (cancelEventArgs: CancelEventArgs) (acc: ExecutionAccumulator<'a>) =
        let result = 
            match cancelEventArgs.Cancel, acc with
            | true, _ ->
                GeneralCancelFailure |> GeneralExecutionFailure
                
            | _, { SetupResult = Some (Ok _); TestResult = Some testResult; TeardownResult = Some (Ok _) } ->
                testResult |> TestExecutionResult
                
            | _, { SetupResult = Some (Error errorValue); TestResult = _; TeardownResult = _ } ->
                errorValue |> SetupExecutionFailure
                
            | _, { SetupResult = _; TestResult = _; TeardownResult = Some (Error errorValue) } ->
                errorValue |> TeardownExecutionFailure

            | _, { SetupResult = _; TestResult = _; TeardownResult = _ } -> failwith "Should not get here"
                
        testLifecycleEvent.Trigger (parent, TestEndExecution result)
        result
    
    member _.Parent with get () = parent
    
    member _.Execute env =
        let cancelEventArgs = CancelEventArgs ()
        
        {
            SetupResult = None
            TestResult = None
            TeardownResult = None 
        }
        |> startExecution cancelEventArgs
        |> startSetup cancelEventArgs
        |> endSetup cancelEventArgs
        |> startTest cancelEventArgs env
        |> endTest
        |> startTearDown cancelEventArgs
        |> endExecution cancelEventArgs
        
    override this.ToString () =
        $"%s{this.Parent.ToString ()}.Executor"
    
    interface ITestExecutor with
        member _.Parent with get () = parent
        member this.Execute env = this.Execute env
        [<CLIEvent>]
        member this.TestLifecycleEvent = testLifecycleEvent.Publish
            
type UnitTest<'a> (containerPath: string, containerName: string, testName: string, tags: TestTag seq, test: 'a -> FrameworkEnvironment -> TestResult, setup: unit -> Result<'a, SetupTeardownFailure>, tearDown: Result<'a, SetupTeardownFailure> -> TestResult option -> Result<unit, SetupTeardownFailure>, location: CodeLocation) =
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
    
    member _.Test<'a> (tags: TagPart, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<'a>, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        match setup, tearDown, tags with
        | SetupPart setup, TeardownPart teardown, TestTagsPart testTags ->
            let test = UnitTest (containerPath, containerName, testName, testTags, testAction, setup, teardown, buildLocation fullPath lineNumber) :> ITest
            tests <- test::tests
            test
    
    member _.Test<'a> (testName: string, tags: TagPart, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<'a>, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        match setup, tearDown, tags with
        | SetupPart setup, TeardownPart teardown, TestTagsPart testTags ->
            let test = UnitTest (containerPath, containerName, testName, testTags, testAction, setup, teardown, buildLocation fullPath lineNumber) :> ITest
            tests <- test::tests
            test
            
    member this.Test<'a> (setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<'a>, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], setup, testAction, tearDown, testName, fullPath, lineNumber)
            
    member this.Test<'a> (testName: string, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<'a>, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], setup, testAction, tearDown, testName, fullPath, lineNumber)
        
    member this.Test<'a> (tags: TagPart, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, setup, testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test<'a> (testName: string, tags: TagPart, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, setup, testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test<'a> (setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (setup, testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test<'a> (testName: string, setup: SetupPart<'a>, testAction: 'a -> FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (setup, testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test (tags: TagPart, testAction: unit -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<unit>, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, SetupPart (fun _ -> Ok ()), testAction, tearDown, testName, fullPath, lineNumber)
        
    member this.Test (testName: string, tags: TagPart, testAction: unit -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<unit>, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, SetupPart (fun _ -> Ok ()), testAction, tearDown, testName, fullPath, lineNumber)
        
    member this.Test (testAction: unit -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<unit>, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], SetupPart (fun _ -> Ok ()), testAction, tearDown, testName, fullPath, lineNumber)
        
    member this.Test (testName: string, testAction: unit -> FrameworkEnvironment -> TestResult, tearDown: TeardownPart<unit>, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], SetupPart (fun _ -> Ok ()), testAction, tearDown, testName, fullPath, lineNumber)
        
    member this.Test (tags: TagPart, testAction: unit -> FrameworkEnvironment -> TestResult, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, SetupPart (fun _ -> Ok ()), testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test (testName: string, tags: TagPart, testAction: unit -> FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (tags, SetupPart (fun _ -> Ok ()), testAction, TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test (testAction: FrameworkEnvironment -> TestResult, [<CallerMemberName; Optional; DefaultParameterValue("")>] testName: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], SetupPart (fun _ -> Ok ()), (fun _ -> testAction), TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
        
    member this.Test (testName: string, testAction: FrameworkEnvironment -> TestResult, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.Test (TestTagsPart [], SetupPart (fun _ -> Ok ()), (fun _ -> testAction), TeardownPart (fun _ _ -> Ok ()), testName, fullPath, lineNumber)
    
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