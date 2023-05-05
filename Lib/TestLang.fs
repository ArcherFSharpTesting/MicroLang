[<AutoOpen>]
module Archer.MicroLang.Lang

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.RunnerTypes
open Archer.MicroLang.Types

let suite = TestContainerBuilder ()

let random = Random ()

let randomInt _ = random.Next ()
let random0To max = random.Next (0, max) 
let ignoreInt _ = randomInt ()
let ignoreString _ = $"%d{randomInt ()}%d{randomInt ()}%d{randomInt ()}"

let ignorePath _ = $"%s{ignoreString ()}.test"

let successfulTest _ = TestSuccess

let successfulUnitSetup _ = Ok ()
let successfulEnvironmentTest _ _ = TestSuccess

let successfulTeardown _ _ = Ok ()

let reportFailures (failures: TestFailContainer list) =
    let rec reportFailures (failures: TestFailContainer list) depth =
        let indent =
            seq {
                for _y in 0..depth do
                    yield "\t"
            }
            |> fun items -> String.Join ("", items)
            
        let rec deconstruct (test: ITest) (failure: TestFailureType) =
            match failure with
            | GeneralFailureType generalTestingFailure ->
                match generalTestingFailure with
                | GeneralFailure message ->
                    test.Location, $"GeneralFailure (%s{message})"
                | GeneralCancelFailure ->
                    test.Location, $"GeneralCancelFailure"
                | GeneralExceptionFailure ex ->
                    test.Location, $"%A{ex}"
            | SetupFailureType setupFailure ->
                match setupFailure with
                | GeneralSetupTeardownFailure (message, codeLocation) ->
                    codeLocation, $"GeneralSetupFailure (%s{message})"
                | SetupTeardownCanceledFailure ->
                    test.Location, "SetupCancelFailure"
                | SetupTeardownExceptionFailure ex ->
                    test.Location, $"%A{ex}"
            | TeardownFailureType teardownFailure ->
                match teardownFailure with
                | GeneralSetupTeardownFailure (message, codeLocation) ->
                    codeLocation, $"GeneralTeardownFailure (%s{message})"
                | SetupTeardownCanceledFailure ->
                    test.Location, "TearDownCanceledFailure"
                | SetupTeardownExceptionFailure ex ->
                    test.Location, $"%A{ex}"
            | TestRunFailureType testFailure ->
                match testFailure with
                | TestIgnored (stringOption, codeLocation) ->
                    let message =
                        match stringOption with
                        | Some value -> value
                        | None -> ""
                        
                    codeLocation, $"Ignored (%s{message})"
                | TestExceptionFailure ex ->
                    test.Location, $"%A{ex}"
                | TestExpectationFailure (testExpectationFailure, codeLocation) ->
                    match testExpectationFailure with
                    | ExpectationOtherFailure message ->
                        codeLocation, message
                    | ExpectationVerificationFailure verificationInfo ->
                        codeLocation, $"VerificationFailure (%A{verificationInfo})"
                    | FailureWithMessage (message, testFailure) ->
                        codeLocation, $"%A{testFailure}  : (%s{message})"
                | CombinationFailure(failureA, failureB) ->
                    let getFailure value =
                        match value with
                        | f, None ->
                            test.Location, deconstruct test (TestRunFailureType f)
                        | f, Some location -> 
                            location, deconstruct test (TestRunFailureType f)
                    
                    let a = getFailure failureA
                    let b = getFailure failureB
                        
                    test.Location, $"CombinationFailure (%A{a}, %A{b})"
            
        failures
        |> List.iter (fun failure ->
            match failure with
            | EmptyFailures -> ()
            | FailedTests tests ->
                tests
                |> List.iter (fun (result, test) ->
                    let location, failure =
                        deconstruct test result
                        
                    printfn $"%s{indent}----------------------"
                    printfn $"%s{indent}%s{test.TestName}"
                    printfn $"%s{indent}\t%A{failure}"
                    printfn ""
                    printfn $"%s{indent}%s{System.IO.Path.Combine (location.FilePath, location.FileName)}(%d{location.LineNumber})"
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
                for _y in 0..depth do
                    yield "\t"
            }
            |> fun items -> String.Join ("", items)

        ignored
        |> List.iter (fun ignored ->
            match ignored with
            | EmptyIgnore -> ()
            | IgnoredTests tests ->
                tests
                |> List.iter (fun (message, location, test) ->
                    let msg =
                        match message with
                        | Some s -> s
                        | None -> ""
                        
                    printfn $"%s{indent}----------------------"
                    printfn $"%s{indent}%s{test.TestName}"
                    printfn $"%s{indent}\tIgnored %s{msg}"
                    printfn ""
                    printfn $"%s{indent}%s{System.IO.Path.Combine (location.FilePath, location.FileName)}(%d{location.LineNumber})"
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
        | FailedTests tests::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countFailures tail
        | FailContainer(_, testFailContainers)::tail ->
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
        | SucceededTests tests::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countSuccesses tail
        | SuccessContainer(_, testSuccessContainers)::tail ->
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
        | IgnoredTests tests::tail ->
            tests
            |> List.length
            |> (+) acc
            |> countIgnored tail
        | IgnoreContainer(_, testIgnoreContainers)::tail ->
            acc
            |> countIgnored testIgnoreContainers
            |> countIgnored tail
        | EmptyIgnore::tail ->
            acc
            |> countIgnored tail
    0
    |> countIgnored ignored
    
let maybeFilterAndReport (filter: (ITest list -> ITest list) option) (runner: IRunner) =
    let startTime = DateTime.Now
    printfn $"Started at %s{startTime.ToShortTimeString ()}"
    let results =
        match filter with
        | None -> runner.Run ()
        | Some value -> runner.Run value

    let endTime = DateTime.Now
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

let filterRunAndReport (filter: ITest list -> ITest list) (runner: IRunner) =
    maybeFilterAndReport (Some filter) runner
let runAndReport (runner: IRunner) =
    maybeFilterAndReport None runner