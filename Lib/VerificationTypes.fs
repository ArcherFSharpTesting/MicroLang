module Archer.MicroLang.VerificationTypes

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer.MicroLang.Types.TypeSupport
open Archer

type FailureWithBuilder () =
    member _.OtherTestExecutionFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> OtherFailure
        
    member this.ValidationFailure (expected, actual, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ValidationFailure (
            {
                Expected = expected
                Actual = actual 
            }, fullPath, lineNumber
        )
        
    member _.ValidationFailure (results, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            results,
            buildLocation fullPath lineNumber
        )
        |> VerificationFailure
        
    member _.GeneralSetupTeardownFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    member this.GeneralNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.OtherTestExecutionFailure ("Not Run", fullPath, lineNumber)
        
    member this.NotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ValidationFailure ({ Expected = "Not to have been run"; Actual = "Was run" }, fullPath, lineNumber)
        
type FailureAsBuilder () =
    let withBuilder = FailureWithBuilder () 
    member this.GeneralSetupFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        withBuilder.GeneralSetupTeardownFailure (message, fullPath, lineNumber)
        |> SetupFailure
        
type FailureBuilder () =
    let withBuilder = FailureWithBuilder ()
    let asBuilder = FailureAsBuilder ()
    
    member _.With with get () = withBuilder
    member _.AsResultOf with get () = asBuilder
        
    [<Obsolete "Use 'With.OtherTestExecutionFailure'">]
    member this.AsGeneralFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.OtherTestExecutionFailure (message, fullPath, lineNumber)
        
    [<Obsolete "Use 'With.ValidationFailure'">]
    member this.AsValidationFailure (expected, actual, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.ValidationFailure (
            {
                Expected = expected
                Actual = actual 
            }, fullPath, lineNumber
        )
        
    [<Obsolete "Use 'With.ValidationFailure'">]
    member this.AsValidationFailure (results, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            results,
            buildLocation fullPath lineNumber
        )
        |> VerificationFailure
        
    [<Obsolete "Use 'As.SetupFailure'">]
    member this.AsSetupFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.AsResultOf.GeneralSetupFailure (message, fullPath, lineNumber)
        
    [<Obsolete "Use 'With.GeneralSetupTeardownFailure'">]
    member this.AsGeneralSetupTeardownFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    [<Obsolete ("Use 'With.GeneralNotRunFailure'")>]
    member this.GeneralNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.GeneralNotRunFailure (fullPath, lineNumber);
        
    [<Obsolete "Use 'With.NotRunValidationFailure'">]
    member this.NotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.NotRunValidationFailure (fullPath, lineNumber)
        
    [<Obsolete "Use 'With.OtherTestExecutionFailure'">]
    member _.AsOtherTestExecutionFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> OtherFailure
        
type ValueComparisonResult<'a> = {
    ExpectedValue: 'a
    Delta: 'a
}

type Expect () =
    let failureBuilder = FailureBuilder ()
    member this.ToBe (expected, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let check actual =
            if actual = expected then TestSuccess
            else
                failureBuilder.With.ValidationFailure ({ Expected = $"%A{expected}"; Actual = $"%A{actual}" }, fullPath, lineNumber)
                |> TestFailure
                
        check
        
    member this.ToBeTrue (actual: bool, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBe (true, fullPath, lineNumber) actual
        
    member this.ToBeFalse (actual: bool, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBe (false, fullPath, lineNumber) actual
            
        
    member _.ToBeIgnored (message: string option, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let ignoreIt ignoreValue =
            ignoreValue |> ignore
            (
                message,
                buildLocation fullPath lineNumber
            )
            |> TestIgnored
            
        ignoreIt
        
    member this.ToBeIgnored (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBeIgnored (Some message, fullPath, lineNumber)