module LazyTests.MultiThreadedTests

open NUnit.Framework
open FsUnit
open Lazy
open System.Threading
open Microsoft.FSharp.Collections

let multiThreadedTestFunc isLockFreeImplementation =
    let testIterationStarted = new ManualResetEvent(false)

    for i = 0 to 5 do
        testIterationStarted.Reset() |> ignore
        let mutable counter = 0

        let supplier =
            fun () ->
                counter <- counter + 1
                counter

        let lazyObject : ILazy<int> =
            if isLockFreeImplementation then
                LockFreeLazy<int>(supplier)
            else
                MultiThreadedLazy<int>(supplier)

        let asyncTask =
            async {
                testIterationStarted.WaitOne() |> ignore
                lazyObject.Get() |> should equal 1
            }

        let tasksArray =
            Seq.init 10 (fun (num) -> asyncTask)
            |> Seq.map (fun (asyncElement) -> Async.StartAsTask asyncElement)

        testIterationStarted.Set() |> ignore
        tasksArray |> Seq.map (fun task -> task.Wait()) |> ignore

[<Test>]
let MultiThreadedTest () = multiThreadedTestFunc false

[<Test>]
let LockFreeMultiThreadedTest () = multiThreadedTestFunc true
