namespace BracketsBalance

open Stack;

module BracketsBalance =
    let closingBrackets = Map [ (')', '('); (']', '['); ('}', '{') ];
    let openingBrackets = Set ( seq {'('; '['; '{' } );

    let checkBracketsBalance string =
        let stack = []
        let rec loop i isCorrect stack =
            if not isCorrect then false, stack
            elif i >= String.length string then isCorrect, stack
            else
                let symbol = string[i]
                if Set.contains symbol openingBrackets then
                    loop (i + 1) isCorrect <| Stack.push stack symbol
                elif Map.containsKey symbol closingBrackets then
                    if Stack.isEmpty stack || Option.defaultValue ' ' (Stack.top stack) <> Map.find symbol closingBrackets
                    then false, stack
                    else
                        loop (i + 1) isCorrect <| Stack.pop stack
                else
                    loop (i + 1) isCorrect stack
        let result, stack = loop 0 true stack
        if result then
            if Stack.isEmpty stack then true else false
        else
            result
