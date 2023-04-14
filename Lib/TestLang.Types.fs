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
    let successfulTest _ = TestSuccess
    let successfulSetup _ = SetupSuccess
    let successfulTeardown _ = TeardownSuccess
    
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
        
type UnitTestExecutor (parent: ITest, setup: unit -> SetupResult, test: FrameworkEnvironment -> TestResult, tearDown: unit -> TeardownResult) =
    let testLifecycleEvent = Event<TestExecutionDelegate, TestEventLifecycle> ()
    
    let maybeDo getEventArgs predicate ok fail ((previousResult, cancelEventArgs: CancelEventArgs) as resultArgs) =
        let args = getEventArgs resultArgs
        testLifecycleEvent.Trigger (parent, args)
            
        match previousResult, cancelEventArgs.Cancel with
        | Some previousResult, false ->
            if predicate previousResult then
                previousResult |> ok |> Some, cancelEventArgs
            else
                previousResult |> fail |> Some, cancelEventArgs
        | _ -> None, cancelEventArgs
    
    let startExecution cancelEventArgs =
        maybeDo (snd >> TestStartExecution) (fun _ -> true) (fun () -> ()) (fun () -> ()) (Some (), cancelEventArgs)
        
    let startSetup =
        maybeDo (snd >> TestStartSetup) (fun _ -> true) setup successfulSetup
        
    let endSetup =
        let buildArgs (previousResult, cancelEventArgs) =
            let result =
                match previousResult with
                | Some result -> result
                | None -> SetupTeardownCanceledFailure |> SetupFailure
                
            TestEndSetup (result, cancelEventArgs)
            
        maybeDo buildArgs (fun _ -> true) id id
         
    let runTest environment =
        maybeDo (snd >> TestStart) (fun (result, _) -> result = SetupSuccess) (fun _ -> environment |> test |> TestExecutionResult) (fun setupResult -> setupResult |> SetupE)
        
    member _.Parent with get () = parent
    
    member _.Execute env =
        let result = TestSuccess
        
        // CancelEventArgs ()
        // |> raiseStartExecution
        // |> raiseStartSetup writeResult
        // |> raiseEndSetup result
        // |> raiseTestStart result writeResult env
        // |> raiseTestEnd result
        // |> raiseStartTearDown writeResult
        // |> raiseEndExecution result
        // |> ignore
        
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
    | SetupPart of (unit -> SetupResult)
    | TeardownPart of (unit -> TeardownResult)
    | Both of setup: (unit -> SetupResult) * tearDown: (unit -> TeardownResult)
            
type UnitTest (containerPath: string, containerName: string, testName: string, tags: TestTag seq, test: FrameworkEnvironment -> TestResult, testParts: TestPart, location: CodeLocation) =
    let setup, tearDown =
        match testParts with
        | EmptyPart -> successfulSetup, successfulTeardown
        | SetupPart setup -> setup, successfulTeardown
        | TeardownPart tearDown -> successfulSetup, tearDown
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