module BracketsBalanceTests

open BracketsBalance.BracketsBalance
open NUnit.Framework
open FsUnit

[<Test>]
let ``Check the correctness of the simple balanced brackets sequence should return true`` () =
    let sequence () =  "()"
    checkBracketsBalance (sequence ()) |> should be True

[<Test>]
let ``Check the correctness of the balanced sequence with different types of brackets should return true`` () =
    let sequence () = "([{}])"
    checkBracketsBalance (sequence ()) |> should be True

[<Test>]
let ``Check the correctness of the balanced brackets sequence with other characters should return true`` () =
    let sequence () = "fjhgh(kjhkjh[bjhg]gghjg{}{}jkhjk)"
    checkBracketsBalance (sequence ()) |> should be True

[<Test>]
let ``Check the correctness of the sequence without brackets should return true`` () =
    let sequence () = "hhgcxzgfsghyjghoi;lkjhbvtyrjgsfxctyujhguik"
    checkBracketsBalance (sequence ()) |> should be True

[<Test>]
let ``Check the correctness of the empty sequence should return true`` () =
    let sequence () = ""
    checkBracketsBalance (sequence ()) |> should be True

[<Test>]
let ``Check the correctness of the sequence with the closing bracket should return false`` () =
    let sequence () = ")"
    checkBracketsBalance (sequence ()) |> should be False

[<Test>]
let ``Check the correctness of the sequence with the opening bracket should return false`` () =
    let sequence () = "("
    checkBracketsBalance (sequence ()) |> should be False

[<Test>]
let ``Check the correctness of the unbalanced brackets sequence should return false`` () =
    let sequence () = "()()([{]})"
    checkBracketsBalance (sequence ()) |> should be False

[<Test>]
let ``Check the correctness of the unbalanced brackets sequence with other characters should return false`` () =
    let sequence () = "fjhgh(kjhkjh[bjhg]gghjg{}{)jkhjk)"
    checkBracketsBalance (sequence ()) |> should be False