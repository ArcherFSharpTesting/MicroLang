module Archer.MicroLang.Tests.``expects ToBeTypeOf``

open System.Collections.Generic
open Archer.MicroLang

let private container = suite.Container ()

let ``<IEnumerable> should succeed for string`` =
    container.Test (
        fun _ ->
            "Hello"
            |> expects.ToBeOfType<System.Collections.IEnumerable> 
    )

let ``<IEnumerable<char>> should succeed for string`` =
    container.Test (
        fun _ ->
            "Hello"
            |> expects.ToBeOfType<IEnumerable<char>> 
    )

let ``<obj> should succeed for string`` =
    container.Test (
        fun _ ->
            "Hello"
            |> expects.ToBeOfType<obj> 
    )

let ``<obj> should succeed for int`` =
    container.Test (
        fun _ ->
            159
            |> expects.ToBeOfType<obj> 
    )
    
let ``Test Cases`` = container.Tests