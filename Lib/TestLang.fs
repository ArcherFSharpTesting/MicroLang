[<AutoOpen>]
module Archer.MicroLang.Lang

open Archer
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.FrameworkTypes
open Archer.MicroLang.Types

let suite = TestContainerBuilder ()

let randomInt _ = System.Random().Next ()
let ignoreInt _ = randomInt ()
let ignoreString _ = $"%d{randomInt ()}%d{randomInt ()}%d{randomInt ()}"

let ignorePath _ = $"%s{ignoreString ()}.test"

let successfulTest _ = TestSuccess
        
let buildDummyTest (testAction: (FrameworkEnvironment -> TestResult) option) (parts: TestPart option) =
    let c = suite.Container (ignoreString (), ignoreString ())
        
    match parts, testAction with
    | None, None -> c.Test (ignoreString (), successfulTest, EmptyPart, ignoreString (), ignoreInt ())
    | None, Some action -> c.Test (ignoreString (), action, EmptyPart, ignoreString (), ignoreInt ())
    | Some part, None -> c.Test (ignoreString (), successfulTest, part, ignoreString (), ignoreInt ())
    | Some part, Some action -> c.Test (ignoreString (), action, part, ignoreString (), ignoreInt ())
    
let buildDummyExecutor (testAction: (FrameworkEnvironment -> TestResult) option) (parts: TestPart option) =
    let test = buildDummyTest testAction parts
    
    test.GetExecutor ()
    
let notRunGeneralFailure = "Not Run" |> GeneralFailure |> TestFailure

let notRunExpectation = { Expected = "Not to have been run"; Actual = "Was run" } |> VerificationFailure

let notRunValidationFailure = notRunExpectation |> TestFailure

let reportFailures (failures: (TestingFailure * ITest) list) =
    failures
    |> List.groupBy (fun (_, test) -> test.ContainerPath, test.ContainerName)
    |> List.iter (fun ((containerPath, containerName), results) ->
        printfn $"%s{containerPath}"
        printfn $"\t%s{containerName}"

        results
        |> List.iter (fun (failure, test) ->
            printfn $"\t\t%s{test.TestName}"
            printfn $"\t\t\t%A{failure}"
            printfn ""
            printfn $"\t\t%s{System.IO.Path.Combine (test.FilePath, test.FileName)}(%d{test.LineNumber})"
            printfn "\t\t----------------------"
        )
    )

let runAndReport (framework: IFramework) =
    let startTime = System.DateTime.Now
    printfn $"Started at %s{startTime.ToShortTimeString ()}"
    let results = framework.Run ()

    let endTime = System.DateTime.Now
    printfn $"Ended at %s{endTime.ToShortTimeString ()}"

    let ignored =
        results.Failures
        |> List.filter (fun (result, _) ->
            match result with
            | IgnoredFailure _
            | CancelFailure -> true
            | _ -> false
        )
        
    let failures =
        results.Failures
        |> List.filter (fun (result, _) ->
            match result with
            | IgnoredFailure _
            | CancelFailure -> false
            | _ -> true
        )
    
    let failureCount = failures |> List.length
        
    printfn $"\nTests Passing: %d{results.Successes |> List.length}, Ignored: %d{ignored |> List.length} Failing: %d{failureCount}\n"

    failures
    |> reportFailures

    printfn ""

    ignored
    |> reportFailures

    printfn $"\n\nTotal Time: %A{endTime - startTime}"
    printfn $"\nSeed: %d{results.Seed}"

    printfn "\n"

    exit failureCount