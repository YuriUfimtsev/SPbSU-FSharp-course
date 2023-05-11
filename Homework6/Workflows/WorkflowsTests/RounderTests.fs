module RounderTests

open NUnit.Framework
open FsUnit
open RounderBuilder

[<Test>]
let ``Calculating of division  of '1/6' by '3.5' with rounding in 3 digits should return 0.048`` () =
    let rounder = RounderBuilder
    rounder 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    |> (-) 0.048
    |> abs
    |> should lessThan 0.00001

[<Test>]
let ``Calculating of multiplication of '3.555' by '3.5554' with rounding in 5 digits should return 12.63945`` () =
    let rounder = RounderBuilder
    rounder 5 {
        let! a = 3.555
        let! b = 3.5554
        return a * b
    }
    |> (-) 12.63945
    |> abs
    |> should lessThan 0.00001

[<Test>]
let ``Calculating of multiplication of '15.5' by '1.2' with rounding in 0 digits should return 19`` () =
    let rounder = RounderBuilder
    rounder 0 {
        let! a = 15.5
        let! b = 1.2
        return a * b
    }
    |> (-) 19.0
    |> abs
    |> should lessThan 0.00001
