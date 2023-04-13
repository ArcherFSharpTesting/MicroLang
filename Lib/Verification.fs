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
        |> TestExecutionFailure
        
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
        |> TestExecutionFailure
        
    member this.AsSetupFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTearDownFailure
        |> SetupFailure
        
    member this.GeneralNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsGeneralFailure ("Not Run", fullPath, lineNumber)
        
    member this.NotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsValidationFailure ({ Expected = "Not to have been run"; Actual = "Was run" }, fullPath, lineNumber)
        
    member _.ToBeIgnored (message: string option, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> Ignored
        
    member this.ToBeIgnored (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBeIgnored (Some message, fullPath, lineNumber)
        
let expects = Expect ()
                

let combineResultIgnoring defaultError a b =
    match a, b with
    | var, _ when var = defaultError -> b
    | _, var when var = defaultError -> a
    | TestSuccess, _ -> b
    | _, TestSuccess -> a
    | TestFailure (TestExecutionFailure tfa), TestFailure (TestExecutionFailure tfb) -> CombinationFailure (tfa, tfb) |> TestExecutionFailure |> TestFailure
    | TestFailure (TestExecutionFailure _) as failure, _
    | _, (TestFailure (TestExecutionFailure _) as failure) -> failure
    | TestFailure CancelFailure as failure, _
    | _, (TestFailure CancelFailure as failure) -> failure
    | TestFailure (ExceptionFailure _) as failure, _
    | _, (TestFailure (ExceptionFailure _) as failure) -> failure
    | TestFailure (SetupFailure _) as failure, _
    | _, (TestFailure (SetupFailure _) as failure) -> failure
    | TestFailure (TearDownFailure _) as failure, _
    | _, (TestFailure (TearDownFailure _) as failure) -> failure
    | Ignored _ as ing, _
    | _, (Ignored _ as ing) -> ing
    
    //| TestFailure tfa, TestFailure tfb -> CombinationFailure (tfa, tfb) |> TestFailure
    
let andResult = combineResultIgnoring TestSuccess

let orResult a b =
    match a, b with
    | TestFailure _, other
    | other, TestFailure _ -> other
    | _ -> a

let combineError = combineResultIgnoring TestSuccess
        
let withMessage message result =
    match result with
    | TestFailure (TestExecutionFailure f) -> FailureWithMessage (message, f) |> TestExecutionFailure |> TestFailure
    | TestSuccess
    | Ignored _
    | TestFailure _ -> result