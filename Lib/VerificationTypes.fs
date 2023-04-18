﻿module Archer.MicroLang.VerificationTypes

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Archer.MicroLang.Types.TypeSupport
open Archer


type FailureWithBuilder () =
    member _.SetupTeardownGeneralFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message,
            buildLocation fullPath lineNumber
        )
        |> GeneralSetupTeardownFailure
        
    member _.TestExecutionExceptionFailure ex = ex |> TestExceptionFailure
        
    member _.SetupTeardownCanceledFailure () = SetupTeardownCanceledFailure
    
    member _.SetupTeardownExceptionFailure ex = ex |> SetupTeardownExceptionFailure
    
    member _.TestValidationFailure<'a> (expected: 'a, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let validate (actual: 'a) =
            (
                {
                    Expected = $"%A{expected}"
                    Actual = $"%A{actual}" 
                } |> ExpectationVerificationFailure,
                buildLocation fullPath lineNumber
            )
            |> TestExpectationFailure
            
        validate
        
    member _.TestValidationFailure (results, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            results |> ExpectationVerificationFailure,
            buildLocation fullPath lineNumber
        )
        |> TestExpectationFailure
        
    member _.TestOtherExpectationFailure (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        (
            message |> ExpectationOtherFailure,
            buildLocation fullPath lineNumber
        )
        |> TestExpectationFailure
        
    member this.TestExecutionNotRunFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.TestOtherExpectationFailure ("Should not run", fullPath, lineNumber)
        
    member this.TestExecutionNotRunValidationFailure ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.TestValidationFailure ("Not To Run", fullPath, lineNumber) "Was run"
 
type SetupTeardownFailureBuilder<'a> (resultType: SetupTeardownFailure -> 'a) =
    let withBuilder = FailureWithBuilder ()
    member _.GeneralFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        withBuilder.SetupTeardownGeneralFailure (message, fullPath, lineNumber)
        |> resultType
        
    member _.CanceledFailure () =
        withBuilder.SetupTeardownCanceledFailure ()
        |> resultType
    
    member _.ExceptionFailure ex =
        ex
        |> withBuilder.SetupTeardownExceptionFailure
        |> resultType
        
        
type TestExecutionFailureResultBuilder () =
    let failureWith = FailureWithBuilder ()
    
    member _.VerificationFailure<'a> (expected: 'a, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        failureWith.TestValidationFailure (expected, fullPath, lineNumber) >> TestFailure
        
    member _.ValidationFailure (result, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        failureWith.TestValidationFailure (result, fullPath, lineNumber) |> TestFailure
    
    member _.TestExceptionFailure = failureWith.TestExecutionExceptionFailure >> TestFailure
    
    member _.OtherExpectationFailure (message, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        failureWith.TestOtherExpectationFailure (message, fullPath, lineNumber) |> TestFailure
    
type FailureAsResultBuilder () =
    let setupFailureResultBuilder = SetupTeardownFailureBuilder SetupFailure
    let tearDownFailureResultBuilder = SetupTeardownFailureBuilder TeardownFailure
    
    let testExecutionFailureBuilder = TestExecutionFailureResultBuilder ()
    
    member _.SetupResultOf with get () = setupFailureResultBuilder
    member _.TeardownResultOf with get () = tearDownFailureResultBuilder
    member _.TestExecutionResultOf with get () = testExecutionFailureBuilder
        
type FailureBuilder () =
    let withBuilder = FailureWithBuilder ()
    let asBuilder = FailureAsResultBuilder ()
    
    member _.With with get () = withBuilder
    member _.As with get () = asBuilder
        
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
                failureBuilder.With.TestValidationFailure ({ Expected = $"%A{expected}"; Actual = $"%A{actual}" }, fullPath, lineNumber)
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
            |> TestFailure
            
        ignoreIt
        
    member this.ToThrow (f, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        try
            f () |> ignore
            this.NotToBeCalled (fullPath, lineNumber) |> Error
        with
        | ex -> Ok ex
        
    member _.NotToBeCalled ([<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        failureBuilder.With.TestOtherExpectationFailure ("Expected not to be called but was", fullPath, lineNumber)
        |> TestFailure
        
    member this.ToBeIgnored (message: string, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        this.ToBeIgnored (Some message, fullPath, lineNumber)
        
    member _.ToBeOfType<'T> (value, [<CallerFilePath; Optional; DefaultParameterValue("")>] fullPath: string, [<CallerLineNumber; Optional; DefaultParameterValue(-1)>]lineNumber: int) =
        let tType = typeof<'T>
        
        if tType.IsInstanceOfType value then
            TestSuccess
        else
            failureBuilder.With.TestValidationFailure ({ Expected = $"%A{tType}"; Actual = $"%A{value.GetType ()}" }, fullPath, lineNumber) |> TestFailure