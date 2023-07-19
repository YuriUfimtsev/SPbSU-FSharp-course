module PointFree

open NUnit.Framework
open FsCheck

let sourceFunc x l = List.map (fun y -> y * x) l
//let func x l = List.map (fun y -> y * x) l
//let func x = List.map (fun y -> y * x)
//let func x = List.map (fun y -> (*) y x)
//let func x = List.map << (fun y -> (*) y) <| x
//let func = List.map << (fun y -> (*) y)
let func = List.map << (*)

[<Test>]
let ``Are the two functions the same`` () =
    let isFuncCorrect x ls = func x ls = sourceFunc x ls
    Check.QuickThrowOnFailure isFuncCorrect