[<Microsoft.FSharp.Core.AutoOpen>]
module Archer.MicroLang.Tests.TestHelpers

open System
open Archer
open Archer.CoreTypes.InternalTypes
open Archer.MicroLang.Lang

let getNoFrameworkInfoFromExecution (test: ITestExecutor) =
    {
        FrameworkName = "No Framework"
        FrameworkVersion = Version "0.0.0.0"
        TestInfo = test.Parent 
    }
    
let ignoreLocation () = {
    FilePath = ignoreString ()
    FileName = ignoreString ()
    LineNumber = ignoreInt ()
}