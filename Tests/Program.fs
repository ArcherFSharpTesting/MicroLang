module Archer.MicroLang.Tests.Program
// For more information see https://aka.ms/fsharp-console-apps

open Archer.Bow
open Archer.MicroLang.Tests
open Archer.CoreTypes

let framework = bow.Framework ()

// These tests test the testing environment used to test the framework
let testDoublesTests =
    [
        ``UnitTest Base Case``.``Test Cases``
        ``UnitTestExecutor Happy Path``.``Test Cases``
        ``UnitTestExecutor With a Failing Test``.``Test Cases``
        ``UnitTestExecutor StartExecution``.``Test Cases``
        ``UnitTestExecutor StartSetup``.``Test Cases``
        ``UnitTestExecutor EndSetup``.``Test Cases``
        ``UnitTestExecutor StartTest``.``Test Cases``
        ``UnitTestExecutor EndTest``.``Test Cases``
        ``UnitTestExecutor StartTearDown``.``Test Cases``
        ``UnitTestExecutor EndExecution``.``Test Cases``
    ]
    |> List.concat

let frameWorkTests =
    [
    ]
    |> List.concat
    
[
    testDoublesTests
    frameWorkTests
]
|> List.concat
|> framework.AddTests

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
|> List.iter (fun (result, test) ->
    printfn $"%s{test.TestFullName}\n\t%A{result}\n\t\t%s{test.FilePath} %d{test.LineNumber}"
)

printfn ""

ignored
|> List.iter (fun (result, test) ->
    printfn $"%s{test.TestFullName}\n%A{result}\n\t%s{test.FilePath} %d{test.LineNumber}"
)

printfn $"\n\nTotal Time: %A{endTime - startTime}"
printfn $"\nSeed: %d{results.Seed}"

printfn "\n"

exit failureCount