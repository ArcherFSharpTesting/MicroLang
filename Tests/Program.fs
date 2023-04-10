module Archer.MicroLang.Tests.Program
// For more information see https://aka.ms/fsharp-console-apps

open Archer.Bow
open Archer.MicroLang.Tests
open Archer.MicroLang.Lang

let framework = bow.Framework ()

// These tests test the testing environment used to test the framework
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
|> framework.AddTests
|> runAndReport