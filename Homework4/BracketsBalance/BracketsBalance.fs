namespace BracketsBalance

open Stack;

module BracketsBalance =
    let closingBrackets = Map [ (')', '('); (']', '['); ('}', '{') ];
    let openingBrackets = Set ( seq {'('; '['; '{' } );

    let checkBracketsBalance string =
        let stack = []
        let rec loop i isCorrect stack =
            if not isCorrect then false
            elif i = String.length string - 1 then isCorrect
            else
                let symbol = string[i]
                if Set.contains symbol openingBrackets then
                    loop (i + 1) isCorrect <| Stack.push stack symbol
                elif Map.containsKey symbol closingBrackets then
                    if Stack.isEmpty stack || Option.defaultValue ' ' (Stack.top stack) <> Map.find symbol closingBrackets
                    then false
                    else
                        loop (i + 1) isCorrect <| Stack.pop stack
                else
                    loop (i + 1) isCorrect stack
        if not (Stack.isEmpty stack) then false else true