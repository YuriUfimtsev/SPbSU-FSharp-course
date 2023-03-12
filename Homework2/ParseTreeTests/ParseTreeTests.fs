module ParseTreeTests

open NUnit.Framework
open FsUnit
open ParseTree.ParseTree

let standartTestTree () = Tree (Addition, Tree (Multiplication, Tip 5, Tip 6), Tree (Subtraction, Tree(Division, Tip 10, Tip 5), Tip 80))
let standartTestTreeValue () = -48
let testTreeWithDivisionByZero () = Tree (Division, Tip 5, Tip 0)
let testTip () = Tip 67

[<Test>]
let ``Calculate standart test tree value`` () =
    calculateExpression (standartTestTree ()) |> should equal (standartTestTreeValue ())

[<Test>]
let ``Calculate tip`` () =
    calculateExpression(testTip ()) |> should equal 67

[<Test>]
let ``Calculate tree with division by zero, should raise nullArg exception`` () =
    (fun () -> calculateExpression (testTreeWithDivisionByZero ()) |> ignore) |> should throw typeof<System.ArgumentNullException>