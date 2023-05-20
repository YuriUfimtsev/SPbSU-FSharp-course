module LazyTests.MultiThreadedTests

open NUnit.Framework
open FsUnit
open Lazy
open System.Threading
open Microsoft.FSharp.Collections

let multiThreadedTestFunc (testLazyObject : ILazy<obj>) (manualResetEvent : ManualResetEvent) (counter : int ref) =
    for i = 0 to 5 do
        manualResetEvent.Reset() |> ignore

        let asyncTask = async { return testLazyObject.Get() }

        let tasksArray =
            Seq.init 10 (fun _ -> asyncTask)
            |> Seq.map (fun asyncElement -> Async.StartAsTask asyncElement)

        manualResetEvent.Set() |> ignore
        let taskResults = tasksArray |> Seq.map (fun task -> task.Result)
        let goldenObj = Seq.item 0 taskResults

        taskResults
        |> Seq.forall (fun object -> obj.Equals(object, goldenObj))
        |> should be True

        counter.Value |> should equal 1

[<Test>]
let MultiThreadedTest () =
    let testIterationStarted = new ManualResetEvent(false)

    for i = 0 to 5 do
        testIterationStarted.Reset() |> ignore
        let mutable counter = ref 0

        let supplier () =
            testIterationStarted.WaitOne() |> ignore
            Interlocked.Increment counter |> ignore
            obj ()

        let lazyObject : ILazy<obj> = BlockingLazy<obj>(supplier)

        multiThreadedTestFunc lazyObject testIterationStarted counter

[<Test>]
let LockFreeMultiThreadedTest () =
    let testIterationStarted = new ManualResetEvent(false)

    for i = 0 to 5 do
        testIterationStarted.Reset() |> ignore
        let mutable counter = ref 0

        let supplier () =
            testIterationStarted.WaitOne() |> ignore
            Interlocked.Increment counter |> ignore
            obj ()

        let lazyObject : ILazy<obj> = LockFreeLazy<obj>(supplier)

        multiThreadedTestFunc lazyObject testIterationStarted counter
