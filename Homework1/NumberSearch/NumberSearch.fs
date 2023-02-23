open System

let readList lsLength =
    let rec loop lsLength acc i =
        if i >= lsLength then acc
        else loop lsLength ((Console.ReadLine() |> int) :: acc) (i + 1)
    let revertList initialLs =
        let rec loop initialLs acc =
            match initialLs with
            | head::tail -> loop tail (head::acc)
            | [] -> acc
        loop initialLs []
    loop lsLength [] 0 |> revertList

let findInList ls requestedElement =
    let rec loop ls requestedElement i =
        match ls with
        | head::tail ->
            if head = requestedElement then i
            else loop tail requestedElement (i + 1)
        | [] -> -1
    loop ls requestedElement 0

printfn "Enter the length of list:"
let lsLength = Console.ReadLine() |> int
printfn "Enter list elements:"
let ls = readList lsLength
printfn "Enter the element that you want to try to find in the list:"
let requestedElement = Console.ReadLine() |> int
printfn "Element index in the list: %d" (findInList ls requestedElement)