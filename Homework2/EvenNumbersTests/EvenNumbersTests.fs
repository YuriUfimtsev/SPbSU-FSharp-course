module EvenNumbersTests

open NUnit.Framework
open FsUnit
open FsCheck
open EvenNumbers.CountEvenNumbers

[<Test>]
let ``Based on map count of even numbers in list from 1 to 10, should return 5`` () =
    let ls = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    CountEvenNumbersWithMap ls |> should equal 5

[<Test>]
let ``Check based on map and filter counting even numbers functions equality``() =
    let areMapAndFilterCountingEvenNumbersFunctionsEqual (ls:list<int>) =
        CountEvenNumbersWithMap ls = CountEvenNumbersWithFilter ls
    Check.QuickThrowOnFailure areMapAndFilterCountingEvenNumbersFunctionsEqual

[<Test>]
let ``Check based on filter and fold counting even numbers functions equality``() =
    let areFilterAndFoldCountingEvenNumbersFunctionsEqual (ls:list<int>) =
        CountEvenNumbersWithFilter ls = CountEvenNumbersWithFold ls
    Check.QuickThrowOnFailure areFilterAndFoldCountingEvenNumbersFunctionsEqual
