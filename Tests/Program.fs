﻿module Archer.MicroLang.Tests.Program
// For more information see https://aka.ms/fsharp-console-apps

open Archer.Bow
open Archer.MicroLang.Tests
open Archer.MicroLang.Lang

let framework = bow.Framework ()

// These tests test the testing environment used to test the framework
[
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
|> List.concat
|> framework.AddTests
|> runAndReport