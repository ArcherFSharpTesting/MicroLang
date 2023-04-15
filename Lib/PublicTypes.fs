[<AutoOpen>]
module Archer.MicroLang.PublicTypes

open Archer

type SetupPart<'a> = | Setup of setup: (unit -> Result<'a, SetupTeardownFailure>)
type TeardownPart<'a> = | Teardown of teardown: (Result<'a, SetupTeardownFailure> -> TestResult option -> Result<unit, SetupTeardownFailure>)
type TagPart = | TestTags of TestTag list