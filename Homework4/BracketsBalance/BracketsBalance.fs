namespace BracketsBalance

open Stack

module BracketsBalance =
    let closingBrackets = Map [ (')', '('); (']', '['); ('}', '{') ];
    let openingBrackets = Set ( seq {'('; '['; '{' } );

    let checkBracketsBalance string =
        let stack = []

        let rec loop i isCorrect stack =
            if not isCorrect then
                false, stack
            elif i >= String.length string then
                isCorrect, stack
            else
                let character = string[i]
                if Set.contains character openingBrackets then
                    loop (i + 1) isCorrect <| push stack character
                elif Map.containsKey character closingBrackets then
                    if
                        isEmpty stack
                        || Option.defaultValue ' ' (top stack) <> Map.find character closingBrackets
                    then
                        false, stack
                    else
                        loop (i + 1) isCorrect <| pop stack
                else
                    loop (i + 1) isCorrect stack

        let result, stack = loop 0 true stack
        if result then
            if isEmpty stack then true else false
        else
            result
