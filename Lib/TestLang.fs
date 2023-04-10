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

let reportFailures (failures: TestFailContainer list) =
    let rec reportFailures (failures: TestFailContainer list) depth =
        let indent =
            seq {
                for y in 0..depth do
                    yield "\t"
            }
            |> fun items -> System.String.Join ("", items)
            
        failures
        |> List.iter (fun failure ->
            match failure with
            | EmptyFailures -> ()
            | FailedTests tests ->
                tests
                |> List.iter (fun (result, test) ->
                    printfn $"%s{indent}----------------------"
                    printfn $"%s{indent}%s{test.TestName}"
                    printfn $"%s{indent}\t%A{result}"
                    printfn ""
                    printfn $"%s{indent}%s{System.IO.Path.Combine (test.FilePath, test.FileName)}(%d{test.LineNumber})"
                )
            | FailContainer(name, testFailContainers) ->
                printfn $"%s{indent}%s{name}"
                reportFailures testFailContainers (depth + 1)
        )
    
    reportFailures failures 0
    
let reportIgnores (ignored: TestIgnoreContainer list) =
    let rec reportIgnores (ignored: TestIgnoreContainer list) depth =
        let indent =
            seq {
                for y in 0..depth do
                    yield "\t"
            }
            |> fun items -> System.String.Join ("", items)

        ignored
        |> List.iter (fun ignored ->
            match ignored with
            | EmptyIgnore -> ()
            | IgnoredTests tests ->
                tests
                |> List.iter (fun (message, test) ->
                    let msg =
                        match message with
                        | Some s -> s
                        | None -> ""
                        
                    printfn $"%s{indent}----------------------"
                    printfn $"%s{indent}%s{test.TestName}"
                    printfn $"%s{indent}\tIgnored %s{msg}"
                    printfn ""
                    printfn $"%s{indent}%s{System.IO.Path.Combine (test.FilePath, test.FileName)}(%d{test.LineNumber})"
                )
            | IgnoreContainer(name, testIgnoreContainers) ->
                printfn $"%s{indent}%s{name}"
                (depth + 1)
                |> reportIgnores testIgnoreContainers
        )
        
    0
    |> reportIgnores ignored
    
let countFailures failures =
    let rec countFailures failures (acc: int) =
        match failures with
        | [] -> acc
        | (FailedTests tests)::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countFailures tail
        | (FailContainer(_, testFailContainers))::tail ->
            acc
            |> countFailures testFailContainers 
            |> countFailures tail
        | EmptyFailures::tail ->
            acc
            |> countFailures tail
            
    0
    |> countFailures failures

let countSuccesses successes =
    let rec countSuccesses successes acc =
        match successes with
        | [] -> acc
        | (SucceededTests tests)::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countSuccesses tail
        | (SuccessContainer(_, testSuccessContainers))::tail ->
            acc
            |> countSuccesses testSuccessContainers
            |> countSuccesses tail
        | EmptySuccesses::tail ->
            acc
            |> countSuccesses tail
            
    0
    |> countSuccesses successes
    
let countIgnored ignored =
    let rec countIgnored ignored acc =
        match ignored with
        | [] -> acc
        | (IgnoredTests tests)::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countIgnored tail
        | (IgnoreContainer(_, testIgnoreContainers))::tail ->
            acc
            |> countIgnored testIgnoreContainers
            |> countIgnored tail
        | EmptyIgnore::tail ->
            acc
            |> countIgnored tail
    0
    |> countIgnored ignored

let runAndReport (framework: IFramework) =
    let startTime = System.DateTime.Now
    printfn $"Started at %s{startTime.ToShortTimeString ()}"
    let results = framework.Run ()

    let endTime = System.DateTime.Now
    printfn $"Ended at %s{endTime.ToShortTimeString ()}"
    
    let failureCount = results.Failures |> countFailures
    let successCount = results.Successes |> countSuccesses
    let ignoredCount = results.Ignored |> countIgnored
        
    printfn $"\nTests Passing: %d{successCount}, Ignored: %d{ignoredCount} Failing: %d{failureCount}\n"

    results.Failures
    |> reportFailures

    printfn ""

    results.Ignored
    |> reportIgnores

    printfn $"\n\nTotal Time: %A{endTime - startTime}"
    printfn $"\nSeed: %d{results.Seed}"

    printfn "\n"

    exit failureCount