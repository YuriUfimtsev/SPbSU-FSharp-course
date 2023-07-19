module ControlWork1Tests

open NUnit.Framework
open FsUnit
open FsCheck
open ControlWork1.ControlWork1

[<Test>]
let ``Standart supermap test with doubling function`` () =
    let testFunc element = [ element; element ]
    let testList = [ 0; 1; 2; 3 ]
    supermap testFunc testList |> should equal [ 0; 0; 1; 1; 2; 2; 3; 3 ]

[<Test>]
let ``Is supermap equal to map test`` () =
    let testFunc element = [ element ]
    let testList = [ 0; 1; 2; 3 ]
    supermap testFunc testList |> should equal [ 0; 1; 2; 3 ]

[<Test>]
let ``Supermap test with applying function to the empty list`` () =
    let testFunc element = [ element; element ]
    let testList = []
    supermap testFunc testList |> should equal []

[<Test>]
let ``Rhomb with '2' side test`` () =
    let expectedRhomb = [ " * "; "***"; " * " ]
    let realRhomb = createRhomb 2
    realRhomb |> should equal expectedRhomb

[<Test>]
let ``Rhomb with '5' side test`` () =
    let expectedRhomb =
        [ "    *    "
          "   ***   "
          "  *****  "
          " ******* "
          "*********"
          " ******* "
          "  *****  "
          "   ***   "
          "    *    " ]

    let realRhomb = createRhomb 5
    realRhomb |> should equal expectedRhomb

[<Test>]
let ``Rhomb with '0' side test`` () =
    let realRhomb = createRhomb 0
    realRhomb |> should be Empty

[<Test>]
let ``Rhomb with negative side test`` () =
    let realRhomb = createRhomb -5
    realRhomb |> should be Empty
