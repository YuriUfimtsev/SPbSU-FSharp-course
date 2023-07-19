module LazyTests.MultiThreadedTests

open System
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open Lazy
open System.Threading
open Microsoft.FSharp.Collections

let multiThreadedTestFunc (testLazyObject : ILazy<obj>) (manualResetEvent : ManualResetEvent) =
    for i = 0 to 5 do
        manualResetEvent.Reset() |> ignore

        let tasksArray = Seq.init 100 (fun _ -> async { return testLazyObject.Get() })

        Console.WriteLine(ThreadPool.ThreadCount)

        let resultAsync = tasksArray |> Async.Parallel
        manualResetEvent.Set() |> ignore
        let taskResults = resultAsync |> Async.RunSynchronously

        let goldenObj = Seq.item 0 taskResults

        taskResults
        |> Seq.forall (fun object -> obj.ReferenceEquals(object, goldenObj))
        |> should be True

[<Test>]
let MultiThreadedTest () =
    let testIterationStarted = new ManualResetEvent(false)

    let counter = ref 0

    let supplier () =
        testIterationStarted.WaitOne() |> ignore
        Interlocked.Increment counter |> ignore
        obj ()

    let lazyObject : ILazy<obj> = BlockingLazy<obj>(supplier)

    multiThreadedTestFunc lazyObject testIterationStarted

    counter.Value |> should equal 1

[<Test>]
let LockFreeMultiThreadedTest () =
    let testIterationStarted = new ManualResetEvent(false)

    let counter = ref 0

    let supplier () =
        testIterationStarted.WaitOne() |> ignore
        Interlocked.Increment counter |> ignore
        obj ()

    let lazyObject = LockFreeLazy<obj>(supplier)

    multiThreadedTestFunc lazyObject testIterationStarted
