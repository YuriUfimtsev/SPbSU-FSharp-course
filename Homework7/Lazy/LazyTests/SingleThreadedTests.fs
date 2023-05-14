module LazyTests.SingleThreadedTests

open NUnit.Framework
open FsUnit
open Lazy
open LazyTests.TestFunctions
open Microsoft.FSharp.Collections

let valueTypeFunctions : (unit -> obj)[] =
    [| (fun () -> TestFunctions.CalculateRandomValue())
       (fun () -> TestFunctions.GuessNumber()) |]

let referenceTypeFunctions : (unit -> obj)[] =
    [| (fun () -> TestFunctions.ConcatenateString())
       (fun () -> TestFunctions.CreateNewObject()) |]

let GetArrayWithLazyObjects (areValueTypeFunctions : bool) : TestCaseData[] =
    let functionsArray =
        if areValueTypeFunctions then
            valueTypeFunctions
        else
            referenceTypeFunctions

    let mutable lazyObjectsArray =
        Array.zeroCreate (Array.length functionsArray |> (*) 3)

    let mutable i = 0
    let mutable j = 0

    while i < (Array.length lazyObjectsArray) do
        lazyObjectsArray[i] <- TestCaseData(SingleThreadedLazy<obj>(functionsArray[j]))
        lazyObjectsArray[i + 1] <- TestCaseData(MultiThreadedLazy<obj>(functionsArray[j]))
        lazyObjectsArray[i + 2] <- TestCaseData(LockFreeLazy<obj>(functionsArray[j]))
        i <- i + 3
        j <- j + 1

    lazyObjectsArray


[<TestCaseSource("GetArrayWithLazyObjects", [| true :> obj |])>]
let CompareValueTypeFunctionsResults (testLazy : ILazy<obj>) =
    let firstCalculation = testLazy.Get()
    let secondCalculation = testLazy.Get()
    let thirdCalculation = testLazy.Get()
    firstCalculation |> should be (sameAs secondCalculation)
    firstCalculation |> should be (sameAs thirdCalculation)

[<TestCaseSource("GetArrayWithLazyObjects", [| false :> obj |])>]
let CompareReferenceTypeFunctionsResults (testLazy : ILazy<obj>) =
    let firstCalculation = testLazy.Get()
    let secondCalculation = testLazy.Get()
    let thirdCalculation = testLazy.Get()
    firstCalculation |> should be (sameAs secondCalculation)
    firstCalculation |> should be (sameAs thirdCalculation)
