namespace ControlWork1

open System

module ControlWork1 =
    let supermap mappingFunc ls =
        let rec loop startedList newList =
            match startedList with
            | head :: tail -> loop tail ((mappingFunc head) @ newList)
            | [] -> newList

        loop ls [] |> List.rev

    let createRhomb sideLength =
        let createLine lineNumber diagonalLength =
            let getRhombSymbolsPositions lineNumber diagonalLength =
                let initialPosition = abs ((diagonalLength - 1) / 2 - lineNumber)
                initialPosition, (diagonalLength - 1) - initialPosition

            let startPosition, stopPosition = getRhombSymbolsPositions lineNumber diagonalLength

            let rec loop i acc =
                if i >= diagonalLength then
                    acc
                else if i >= startPosition && i <= stopPosition then
                    loop (i + 1) ('*' :: acc)
                else
                    loop (i + 1) (' ' :: acc)

            loop 0 [] |> List.map string |> List.reduce (+)

        let diagonalLength = sideLength * 2 - 1

        let rec loop i acc =
            if i >= diagonalLength then
                acc
            else
                loop (i + 1) ((createLine i diagonalLength) :: acc)

        if sideLength <= 0 then [] else loop 0 []
