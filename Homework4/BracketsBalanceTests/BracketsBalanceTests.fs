module BracketsBalanceTests

open BracketsBalance.BracketsBalance
open NUnit.Framework
open FsUnit

[<Test>]
let ``Ñheck the correctness of the simple brackets sequence, should return true`` () =
    let simpleCorrectBracketsSequence () =  "()"
    checkBracketsBalance (simpleCorrectBracketsSequence ()) |> should be True

[<Test>]
let ``Ñheck the correctness of the brackets sequence, should return true`` () =
    let correctBracketsSequence () = "([{}])"
    checkBracketsBalance (correctBracketsSequence ()) |> should be True

[<Test>]
let ``Ñheck the correctness of the brackets sequence among other symbols, should return true`` () =
    let correctBracketsSequenceAmongOtherSymbols () = "fjhgh(kjhkjh[bjhg]gghjg{}{}jkhjk)"
    checkBracketsBalance (correctBracketsSequenceAmongOtherSymbols ()) |> should be True

[<Test>]
let ``Ñheck the correctness of the string without brackets, should return true`` () =
    let stringWithoutBrackets () = "hhgcxzgfsghyjghoi;lkjhbvtyrjgsfxctyujhguik"
    checkBracketsBalance (stringWithoutBrackets ()) |> should be True

[<Test>]
let ``Ñheck the correctness of the empty string, should return true`` () =
    let emptyString () = ""
    checkBracketsBalance (emptyString ()) |> should be True

[<Test>]
let ``Ñheck the correctness of the simple brackets sequence with closing bracket, should return false`` () =
    let incorrectBracketsSequenceWithClosingBracket () = ")"
    checkBracketsBalance (incorrectBracketsSequenceWithClosingBracket ()) |> should be False

[<Test>]
let ``Ñheck the correctness of the simple brackets sequence with opening bracket, should return false`` () =
    let incorrectBracketsSequenceWithOpeningBracket () = "("
    checkBracketsBalance (incorrectBracketsSequenceWithOpeningBracket ()) |> should be False

[<Test>]
let ``Ñheck the correctness of the brackets sequence, should return false`` () =
    let incorrectBracketsSequence () = "()()([{]})"
    checkBracketsBalance (incorrectBracketsSequence ()) |> should be False

[<Test>]
let ``Ñheck the correctness of the brackets sequence among other symbols, should return false`` () =
    let incorrectBracketsSequenceAmongOtherSymbols () = "fjhgh(kjhkjh[bjhg]gghjg{}{)jkhjk)"
    checkBracketsBalance (incorrectBracketsSequenceAmongOtherSymbols ()) |> should be False