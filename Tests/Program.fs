module Archer.MicroLang.Tests.Program

open Archer.Bow
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang.Lang

let runner = bow.Runner ()

// These tests test the testing environment used to test the framework
runner
|> addMany [
    ``UnitTest should``.``Test Cases``
    ``UnitTestExecutor happy path``.``Test Cases``
    ``UnitTestExecutor Failing Test``.``Test Cases``
    ``UnitTestExecutor StartExecution should``.``Test Cases``
    ``UnitTestExecutor StartSetup should``.``Test Cases``
    ``UnitTestExecutor EndSetup should``.``Test Cases``
    ``UnitTestExecutor StartTest should``.``Test Cases``
    ``UnitTestExecutor EndTest should``.``Test Cases``
    ``UnitTestExecutor StartTearDown should``.``Test Cases``
    ``UnitTestExecutor EndExecution should``.``Test Cases``
    ``expects ToBeTypeOf``.``Test Cases``
]
|> runAndReport