open System

printfn "Enter n:"
let n = int (Console.ReadLine())
printfn "Enter m:"
let m = int (Console.ReadLine())

let createList n m =
    let twoInMaxDegree = int (2.0 ** int (n + m))
    let rec loop ls lsElement i =
        if i = 0 then ls
        else loop (lsElement :: ls) (lsElement / 2) (i - 1)

    loop [] twoInMaxDegree (m + 1)

printfn "Result: %A" (createList n m)

