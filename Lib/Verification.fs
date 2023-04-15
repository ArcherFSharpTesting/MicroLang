[<AutoOpen>]
module Archer.MicroLang.Verification

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer.MicroLang.Types.TypeSupport
open Archer

type Expect () =
    member this.ToBe (expected, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let check actual =
            if actual = expected then TestSuccess
            else
                this.AsValidationFailure ({ Expected = $"%A{expected}"; Actual = $"%A{actual}" }, fullPath, lineNumber)
                |> TestFailure
                
        check
        
    member this.ToBeTrue (actual: bool, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBe (true, fullPath, lineNumber) actual
        
    member this.AsGeneralFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> OtherFailure
        
    member this.AsValidationFailure (expected, actual, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsValidationFailure (
            {
                Expected = expected
                Actual = actual 
            }, fullPath, lineNumber
        )
        
    member this.AsValidationFailure (results, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            results,
            buildLocation fullPath lineNumber
        )
        |> VerificationFailure
        
    member this.AsSetupFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsGeneralSetupTeardownFailure (message, fullPath, lineNumber)
        |> SetupFailure
        
    member this.AsGeneralSetupTeardownFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    member this.GeneralNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsGeneralFailure ("Not Run", fullPath, lineNumber)
        
    member this.NotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsValidationFailure ({ Expected = "Not to have been run"; Actual = "Was run" }, fullPath, lineNumber)
        
    member _.AsOtherTestExecutionFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> OtherFailure
        
    member _.ToBeIgnored (message: string option, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> TestIgnored
        
    member this.ToBeIgnored (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBeIgnored (Some message, fullPath, lineNumber)
        
let expects = Expect ()
                

let combineResultIgnoring defaultError a b =
    match a, b with
    | TestFailure TestCanceledFailure as failure, _
    | _, (TestFailure TestCanceledFailure as failure) -> failure

    | TestIgnored _ as ing, _ -> ing

    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a

    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    
    | TestFailure tfa, TestFailure tfb -> CombinationFailure (tfa, tfb) |> TestFailure
    | TestFailure _ as failure, _
    | _, (TestFailure _ as failure) -> failure
    
let andResult = combineResultIgnoring TestSuccess

let orResult a b =
    match a, b with
    | TestFailure _, other
    | other, TestFailure _ -> other
    | _ -> a

let combineError = combineResultIgnoring TestSuccess
        
let withMessage message result =
    match result with
    | TestFailure f -> FailureWithMessage (message, f) |> TestFailure
    | TestSuccess
    | TestIgnored _
    | TestFailure _ -> result