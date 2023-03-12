module PrimeNumbersTests
open System
open NUnit.Framework
open FsUnit
open PrimeNumbers

let generateRandom () = 
    let random = Random()
    random.Next(1, 2000)

let isPrime number =
    let checkLimit = number
    let rec loop i =
        if i = checkLimit then true
        else if number % i = bigint 0 && i <> number && i <> bigint 1 then false
        else loop (i + bigint 1)
    loop (bigint 1)

[<Test>]
let ``Check first ten elements in the sequence with the template`` () =
    let tenActualPrimeNumbers = primeNumberSequence () |> Seq.take 10 |> Seq.toList
    tenActualPrimeNumbers = ([1; 2; 3; 5; 7; 11; 13; 17; 19; 23] |> List.map bigint)
    |> should be True

[<Test>]
let ``Check is the random element in the sequence prime`` () =
    let actualNumber = primeNumberSequence () |> Seq.skip (generateRandom ()) |> Seq.take 1 |> Seq.toList |> List.item(0)
    isPrime actualNumber |> should be True