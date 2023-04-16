[<AutoOpen>]
module Archer.MicroLang.PublicTypes

open Archer

type SetupPart<'a> = | SetupPart of setup: (unit -> Result<'a, SetupTeardownFailure>)
type TeardownPart<'a> = | TeardownPart of teardown: (Result<'a, SetupTeardownFailure> -> TestResult option -> Result<unit, SetupTeardownFailure>)
type TagPart = | TestTagsPart of TestTag list