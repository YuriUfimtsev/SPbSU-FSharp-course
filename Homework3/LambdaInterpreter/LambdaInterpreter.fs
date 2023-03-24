namespace LambdaInterpreter

module LambdaInterpreter =
    type Term =
    | Variable of char
    | Application of Term * Term
    | LambdaAbstraction of char * Term

    let getNextVariable (character : char) =
        char ( (int (character)) + 1)

    let getRightVariableName previousVariable sourceLambdaAbstraction newTerm =
        let rec processTheTerm term variablesCollection =
            match term with
            | Variable var ->
                if not (List.exists (fun (elem, isBound) -> elem = var && isBound) variablesCollection)
                then ((var, false) :: variablesCollection)
                else variablesCollection
            | LambdaAbstraction (lambdaVariable, body) -> processTheTerm body ((lambdaVariable, true) :: variablesCollection)
            | Application (firstTerm, secondTerm) -> (processTheTerm firstTerm variablesCollection) @ (processTheTerm secondTerm variablesCollection)
        let variablesCollection = processTheTerm newTerm []
        if not (List.exists (fun (elem, isBound) -> elem = previousVariable && isBound = false) variablesCollection) then
            previousVariable
        else
            let rec loop variableName variablesList =
                if not (List.exists (fun (elem, isBound) -> elem = variableName && isBound = false) variablesCollection) then variableName
                else loop (getNextVariable(variableName)) variablesList
            loop 'a' variablesCollection

    let rec performSubstitution previousVariable sourceTerm newTerm : Term =
        match sourceTerm, newTerm with
        | Variable a, term when a = previousVariable -> term
        | Variable a, _ when a <> previousVariable -> Variable a
        | Application (firstTerm, secondTerm), term ->
            Application(performSubstitution previousVariable firstTerm term, performSubstitution previousVariable secondTerm term)
        | LambdaAbstraction (lambdaVariable, body), Variable var when lambdaVariable = var -> LambdaAbstraction (lambdaVariable, body)
        | LambdaAbstraction (lambdaVariable, body), term  ->
            let rightVariableName = getRightVariableName lambdaVariable body term
            if rightVariableName <> lambdaVariable
            then LambdaAbstraction (rightVariableName, (performSubstitution lambdaVariable body (Variable rightVariableName)))
            else LambdaAbstraction (lambdaVariable, body)
        | _ -> raise (invalidArg "Term" "Unexpected term")

    let betaConversion expression =
        let rec recursiveBetaConversion expression =
            match expression with
            | Variable variable -> Variable variable
            | LambdaAbstraction (variable, body) -> LambdaAbstraction (variable, body)
            | Application (firstTerm, secondTerm) ->
                match firstTerm, secondTerm with
                | (LambdaAbstraction (variable, body), term) -> performSubstitution variable body term
                | _ -> Application(recursiveBetaConversion firstTerm, recursiveBetaConversion secondTerm)
        recursiveBetaConversion expression

    let betaReduction expression =
        let rec loop lambdaExpression isApplicationWithLambdaAbstractionFound =
            if not isApplicationWithLambdaAbstractionFound then lambdaExpression
            else loop (betaConversion lambdaExpression) isApplicationWithLambdaAbstractionFound
        loop expression true
