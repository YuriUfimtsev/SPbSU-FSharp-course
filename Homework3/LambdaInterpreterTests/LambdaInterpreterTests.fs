module LambdaInterpreterTests

open LambdaInterpreter.LambdaInterpreter
open NUnit.Framework
open FsUnit

// λx.x
let IdCombinator () = LambdaAbstraction ('x', Variable 'x')

// λs.s s
let selfApplicabilityCombinator () = LambdaAbstraction ('s', Application (Variable 's', Variable 's'))

// (λx.y) ((λx.x x x) (λx.x x x))
let expressionForNormalStrategyTest () = Application (LambdaAbstraction ('x', Variable 'y'),
    Application (LambdaAbstraction (('x'), Application (Application (Variable 'x', Variable 'x'), Variable 'x')),
    LambdaAbstraction (('x'), Application (Application (Variable 'x', Variable 'x'), Variable 'x'))))

// (λx.λy.λz.x y z t) λt.y t
let expressionForAlphaConversionTest () = Application (LambdaAbstraction ('x', LambdaAbstraction ('y', LambdaAbstraction (
    'z', Application ( Application (Application (Variable 'x', Variable 'y'), Variable 'z'), Variable 't')))),
    LambdaAbstraction ('t', Application (Variable 'y', Variable 't')))
// λ_.λz.y _ z t (_ != y)
let expectedResultFromAlphaConversionTest () = LambdaAbstraction ('_', LambdaAbstraction ('z',
    Application (Application (Application (Variable 'y', Variable '_'), Variable 'z'), Variable 't')))

// (λz.λy.x y z (λz.r f z)) y
let expressionForBoundVariablesReplacementTest () = Application (LambdaAbstraction ('z',
    LambdaAbstraction ('y', Application (Application (Application (Variable 'x', Variable 'y'),
        Variable 'z'), LambdaAbstraction('z', Application (
        Application (Variable 'r', Variable 'f'), Variable 'z'))))), Variable 'y')
// (λ_.x _ y (λz.r f z)) (_ != y)
let expectedResultFromBoundVariablesReplacementTest () = LambdaAbstraction ('_', Application
    (Application (Application (Variable 'x', Variable '_'), Variable 'y'),
    LambdaAbstraction('z', Application (Application (Variable 'r', Variable 'f'), Variable 'z'))))

// ((λx.λr.λt.r t x) k) ((λp.p r) t)
let expressionForThreeBetaConversionTest () = Application (Application (LambdaAbstraction (
    'x', LambdaAbstraction('r', LambdaAbstraction ('t', Application (
        Application (Variable 'r', Variable 't'), Variable 'x') ))), Variable 'k'), Application (
            LambdaAbstraction ('p', Application (Variable 'p', Variable 'r')), Variable 't'))
// (λ_.t r _ k)
let expectedResultFromthreeBetaConversionTest () = LambdaAbstraction ('_', Application (Application (
        Application (Variable 't', Variable 'r'), Variable '_'), Variable 'k'))

[<Test>]
let ``Apply an IdCombinator and an arbitrary term, check that result is the second term`` () =
    betaReduction (Application (IdCombinator (), (Variable 'c'))) |> should equal (Variable 'c')
    betaReduction (Application (IdCombinator (), (Variable 'x'))) |> should equal (Variable 'x')
    betaReduction (Application (IdCombinator (), (LambdaAbstraction ('y', Variable 'k'))))
        |> should equal (LambdaAbstraction ('y', Variable 'k'))
    betaReduction (Application (IdCombinator (), (Application (Variable 'c', Variable 't'))))
        |> should equal (Application (Variable 'c', Variable 't'))

[<Test>]
let ``Apply a selfApplicabilityCombinator and an IdCombinator, check that result is the IdCombinator`` () =
    betaReduction (Application (selfApplicabilityCombinator (), IdCombinator ())) |> should equal (IdCombinator ())

[<Test>]
let ``Check that beta interpreter implements normal strategy`` () =
    betaReduction (expressionForNormalStrategyTest ()) |> should equal (Variable 'y')

// The expression in match is taken from the expectedResultFromAlphaConversionTest () function
[<Test>]
let ``Check that alpha conversion is implemented`` () =
    let p = betaReduction (expressionForAlphaConversionTest ())
    match betaReduction (expressionForAlphaConversionTest ()) with
    | LambdaAbstraction (firstVariable, LambdaAbstraction ('z',
        Application (Application (Application (Variable 'y', Variable secondVariable),
        Variable 'z'), Variable 't')))
        when firstVariable = secondVariable && firstVariable <> 'y' -> true
    | _ -> false
    |> should be True

// The expression in match is taken from the expectedResultFromBoundVariablesReplacementTest () function
[<Test>]
let ``Check that bound variables aren't replaced during beta reduction`` () =
    match betaReduction (expressionForBoundVariablesReplacementTest ()) with
    | LambdaAbstraction (firstVariable, Application
        (Application (Application (Variable 'x', Variable secondVariable), Variable 'y'),
        LambdaAbstraction('z', Application (Application (Variable 'r', Variable 'f'), Variable 'z'))))
        when firstVariable = secondVariable && firstVariable <> 'y' -> true
    | _ -> false
    |> should be True

// The expression in match is taken from the expectedResultFromthreeBetaConversionTest () function
[<Test>]
let ``Apply three beta conversion`` () =
    let p = betaReduction (expressionForThreeBetaConversionTest ())
    match betaReduction (expressionForThreeBetaConversionTest ()) with
    | LambdaAbstraction (firstVariable, Application (Application (
        Application (Variable 't', Variable 'r'), Variable secondVariable), Variable 'k'))
        when firstVariable = secondVariable && firstVariable <> 'r' && firstVariable <> 't' -> true
    | _ -> false
    |> should be True