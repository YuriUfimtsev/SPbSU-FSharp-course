module StackTests

open BracketsBalance.Stack
open NUnit.Framework
open FsUnit

let emptyStack () = []
let notEmplyIntStack () = [1;5]
let notEmplyCharStack () = ['('; '}']

[<Test>]
let ``isEmpty function test`` () =
    isEmpty (emptyStack ()) |> should be True
    isEmpty (notEmplyIntStack ()) |> should be False
    isEmpty (notEmplyCharStack ()) |> should be False

[<Test>]
let ``Push function test`` () =
    push (emptyStack ()) 8 |> should equal (8 :: emptyStack ())
    push (notEmplyIntStack ()) 15 |> should equal (15 :: notEmplyIntStack ())
    push (notEmplyCharStack ()) '[' |> should equal ('[' :: notEmplyCharStack ())

[<Test>]
let ``Pop from not empty stack should return new stack without top element`` () =
    pop (notEmplyIntStack ()) |> should equal [5]
    pop (notEmplyCharStack ()) |> should equal ['}']

[<Test>]
let ``Pop from empty stack should return empty stack`` () =
    pop (emptyStack ()) |> should equal (emptyStack ())

[<Test>]
let ``Top from not empty stack should return Some of top element`` () =
    Option.defaultValue 0 ( top (notEmplyIntStack ())) |> should equal 1
    Option.defaultValue ' ' (top (notEmplyCharStack ())) |> should equal '('

[<Test>]
let ``Top from empty stack should return None`` () =
    top (emptyStack ()) |> should equal None



