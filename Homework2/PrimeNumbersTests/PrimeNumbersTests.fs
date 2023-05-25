module PrimeNumbersTests
open System
open NUnit.Framework
open FsUnit
open PrimeNumbers.PrimeNumbers

[<Test>]
let ``Check first ten elements in the sequence with the template`` () =
    let tenActualPrimeNumbers = primeNumberSequence () |> Seq.take 10 |> Seq.toList
    tenActualPrimeNumbers = ([1; 2; 3; 5; 7; 11; 13; 17; 19; 23] |> List.map bigint)
    |> should be True
