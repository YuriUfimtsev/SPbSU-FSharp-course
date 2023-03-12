﻿namespace EvenNumbers
open System

module CountEvenNumbers =
    let CountEvenNumbersWithMap ls =
        ls
        |> Seq.map (fun element ->
            if element % 2 = 0 then [element]
            else [])
        |> Seq.concat
        |> Seq.length

    let CountEvenNumbersWithFilter ls =
        ls
        |> Seq.filter (fun element ->
            element % 2 = 0)
        |> Seq.length

    let CountEvenNumbersWithFold ls =
        ls
        |> Seq.fold (fun state element ->
            (+) state <| Math.Abs ((+) element 1) % 2) 0