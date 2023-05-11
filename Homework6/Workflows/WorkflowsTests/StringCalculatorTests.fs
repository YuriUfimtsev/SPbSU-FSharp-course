module StringCalculatorTests

open NUnit.Framework
open FsUnit
open StringCalculatorBuilder

[<Test>]
let ``Calculating of sum of '11' and '23' should return 34`` () =
    let stringCalculator = StringCalculatorBuilder()
    stringCalculator {
        let! x = "11"
        let! y = "23"
        let z = x + y
        return z
    }
    |> should equal (Some 34)

[<Test>]
let ``Calculating of sum of '11' and 'z' should return None`` () =
    let stringCalculator = StringCalculatorBuilder()
    stringCalculator {
        let! x = "11"
        let! y = "z"
        let z = x + y
        return z
    }
    |> should equal None
