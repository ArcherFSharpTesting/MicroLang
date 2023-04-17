module Archer.MicroLang.VerificationTypes

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer.MicroLang.Types.TypeSupport
open Archer


type FailureWithBuilder () =
    member _.TestExecutionOtherFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> OtherFailure
        
    member this.TestExecutionValidationFailure (expected, actual, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.TestExecutionValidationFailure (
            {
                Expected = expected
                Actual = actual 
            }, fullPath, lineNumber
        )
        
    member _.TestExecutionValidationFailure (results, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            results,
            buildLocation fullPath lineNumber
        )
        |> VerificationFailure
        
    member _.SetupTeardownGeneralFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    member _.SetupTeardownCanceledFailure () = SetupTeardownCanceledFailure
    
    member _.SetupTeardownExceptionFailure ex = ex |> SetupTeardownExceptionFailure
        
    member this.TestExecutionNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.TestExecutionOtherFailure ("Not Run", fullPath, lineNumber)
        
    member this.TestExecutionNotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.TestExecutionValidationFailure ({ Expected = "Not to have been run"; Actual = "Was run" }, fullPath, lineNumber)
 
 
type SetupTeardownFailureBuilder<'a> (failureType: SetupTeardownFailure -> 'a) =
    let withBuilder = FailureWithBuilder ()
    member _.GeneralFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        withBuilder.SetupTeardownGeneralFailure (message, fullPath, lineNumber)
        |> failureType
        
    member _.CanceledFailure () =
        withBuilder.SetupTeardownCanceledFailure ()
        |> failureType
    
    member _.ExceptionFailure ex =
        ex
        |> withBuilder.SetupTeardownExceptionFailure
        |> failureType
        
type FailureAsResultBuilder () =
    let withBuilder = FailureWithBuilder ()
    let setupFailureBuilder = SetupTeardownFailureBuilder SetupFailure
    let tearDownFailureBuilder = SetupTeardownFailureBuilder TeardownFailure
    
    member _.SetupResultOf with get () = setupFailureBuilder
    member _.TeardownResultOf with get () = tearDownFailureBuilder
        
type FailureBuilder () =
    let withBuilder = FailureWithBuilder ()
    let asBuilder = FailureAsResultBuilder ()
    
    member _.With with get () = withBuilder
    member _.As with get () = asBuilder
        
    [<Obsolete "Use 'With.OtherTestExecutionFailure'">]
    member this.AsGeneralFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.TestExecutionOtherFailure (message, fullPath, lineNumber)
        
    [<Obsolete "Use 'With.ValidationFailure'">]
    member this.AsValidationFailure (expected, actual, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.TestExecutionValidationFailure (
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
        this.As.SetupResultOf.GeneralFailure (message, fullPath, lineNumber)
        
    [<Obsolete "Use 'With.GeneralSetupTeardownFailure'">]
    member this.AsGeneralSetupTeardownFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    [<Obsolete ("Use 'With.GeneralNotRunFailure'")>]
    member this.GeneralNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.TestExecutionNotRunFailure (fullPath, lineNumber);
        
    [<Obsolete "Use 'With.NotRunValidationFailure'">]
    member this.NotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.With.TestExecutionNotRunValidationFailure (fullPath, lineNumber)
        
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
                failureBuilder.With.TestExecutionValidationFailure ({ Expected = $"%A{expected}"; Actual = $"%A{actual}" }, fullPath, lineNumber)
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