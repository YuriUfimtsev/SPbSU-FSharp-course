namespace LambdaInterpreter

module LambdaInterpreter =
    type Term =
    | Variable of char
    | Application of Term * Term
    | LambdaAbstraction of char * Term

    let getNextVariable (character : char) =
        char (int (character) + 1)

    let getRightVariableName previousVariable newTerm =
        let isVariableContainedInCollectionAsBound variable =
            List.exists (fun (elem, isBound) -> elem = variable && isBound)
        let isVariableContainedInCollectionAsFree variable =
            List.exists (fun (elem, isBound) -> elem = variable && not isBound)
        let rec processTheTerm term variablesCollection =
            match term with
            | Variable var ->
                if not (isVariableContainedInCollectionAsBound var variablesCollection)
                then ((var, false) :: variablesCollection)
                else variablesCollection
            | LambdaAbstraction (lambdaVariable, body) -> processTheTerm body ((lambdaVariable, true) :: variablesCollection)
            | Application (firstTerm, secondTerm) -> processTheTerm firstTerm variablesCollection @ processTheTerm secondTerm variablesCollection
        let variablesCollection = processTheTerm newTerm []
        if not (isVariableContainedInCollectionAsFree previousVariable variablesCollection) then
            previousVariable
        else
            let rec loop variableName variablesList =
                if not (isVariableContainedInCollectionAsFree variableName variablesList) then variableName
                else loop (getNextVariable(variableName)) variablesList
            loop 'a' variablesCollection

    let rec performSubstitution previousVariable sourceTerm newTerm =
        match sourceTerm, newTerm with
        | Variable a, term when a = previousVariable -> term
        | Variable a, _ when a <> previousVariable -> Variable a
        | Application (firstTerm, secondTerm), term ->
            Application(performSubstitution previousVariable firstTerm term, performSubstitution previousVariable secondTerm term)
        | LambdaAbstraction (lambdaVariable, body), Variable var when previousVariable = lambdaVariable -> LambdaAbstraction (lambdaVariable, body)
        | LambdaAbstraction (lambdaVariable, body), term  ->
            let rightVariableName = getRightVariableName lambdaVariable term
            if rightVariableName <> lambdaVariable
            then LambdaAbstraction (rightVariableName, performSubstitution previousVariable
                (performSubstitution lambdaVariable body (Variable rightVariableName)) term)
            else LambdaAbstraction (lambdaVariable, performSubstitution previousVariable body term)
        | _ -> raise (invalidArg "Term" "Unexpected term")

    let betaConversion expression =
        let rec recursiveBetaConversion expression isApplicationWithLambdaAbstractionFound =
            match expression, isApplicationWithLambdaAbstractionFound with
            | Variable variable, isFound -> Variable variable, isFound
            | LambdaAbstraction (variable, body), isFound ->
                let newBody, newIsFound = recursiveBetaConversion body isFound
                LambdaAbstraction (variable, newBody), newIsFound
            | Application (LambdaAbstraction (variable, body), term), isFound -> performSubstitution variable body term, true
            | Application (firstTerm, secondTerm), isFound ->
                let newFirstTerm, isInFirstTermFound = recursiveBetaConversion firstTerm isFound
                let newSecondTerm, isInSecondTermFound = recursiveBetaConversion secondTerm isFound
                Application (newFirstTerm, newSecondTerm), (isInFirstTermFound || isInSecondTermFound)
        recursiveBetaConversion expression false

    let betaReduction expression =
        let rec loop lambdaExpression isApplicationWithLambdaAbstractionFound =
            if not isApplicationWithLambdaAbstractionFound then lambdaExpression
            else
                let newExpression, isFound = betaConversion lambdaExpression
                loop newExpression isFound
        loop expression true
