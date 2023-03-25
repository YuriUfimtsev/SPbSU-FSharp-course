module LambdaInterpreterTests

open LambdaInterpreter.LambdaInterpreter
open NUnit.Framework
open FsUnit

let IdCombinator () = LambdaAbstraction('x', Variable 'x')
let selfApplicabilityCombinator () = LambdaAbstraction('s', Application(Variable 's', Variable 's'))

[<Test>]
let ``Apply an IdCombinator and an arbitrary term, check that result is the second term`` () =
    betaReduction (Application (IdCombinator (), (Variable 'c'))) |> should equal (Variable 'c')
    betaReduction (Application (IdCombinator (), (Variable 'x'))) |> should equal (Variable 'x')
    betaReduction (Application (IdCombinator (), (LambdaAbstraction ('y', Variable 'k')))) |> should equal (LambdaAbstraction ('y', Variable 'k'))
    betaReduction (Application (IdCombinator (), (Application (Variable 'c', Variable 't')))) |> should equal (Application (Variable 'c', Variable 't'))

[<Test>]
let ``Apply a selfApplicabilityCombinator and an IdCombinator, check that result is the IdCombinator`` () =
    betaReduction (Application (selfApplicabilityCombinator (), IdCombinator ())) |> should equal (IdCombinator ())

