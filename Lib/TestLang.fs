[<AutoOpen>]
module Archer.MicroLang.Lang

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.RunnerTypes
open Archer.Logger.Indent
open Archer.Logger.TestFailContainerTransformer
open Archer.Logger.TestIgnoreContainerTransformer
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
    printfn $"Started at %s{DateTime.Now.ToShortTimeString ()}"
    let results =
        match filter with
        | None -> runner.Run ()
        | Some value -> runner.Run value

    printfn $"Ended at %s{DateTime.Now.ToShortTimeString ()}"
    
    let failureCount = results.Failures |> countFailures
    let successCount = results.Successes |> countSuccesses
    let ignoredCount = results.Ignored |> countIgnored
    
    printfn ""

    let indenter = IndentTransformer (0, TwoSpaces)
    
    results.Failures
    |> defaultTestFailContainerAllTransformer indenter
    |> printfn "%s"

    printfn ""

    results.Ignored
    |> defaultAllTestIgnoreContainerTransformer indenter
    |> printf "%s"
        
    printfn ""
    printfn $"\nTests Passing: %d{successCount}, Ignored: %d{ignoredCount} Failing: %d{failureCount}"

    printfn $"\nTotal Time: %A{results.TotalTime}"
    printfn $"\nSeed: %d{results.Seed}"

    printfn "\n"

    exit failureCount

let filterRunAndReport (filter: ITest list -> ITest list) (runner: IRunner) =
    maybeFilterAndReport (Some filter) runner
let runAndReport (runner: IRunner) =
    maybeFilterAndReport None runner