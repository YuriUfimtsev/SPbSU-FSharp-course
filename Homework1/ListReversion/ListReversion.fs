let list = [1..10]
printfn "Source list: %A" list

let reverseList list =
    let rec loop acc supportiveList =
        match supportiveList with
        | head :: tail -> loop (head :: acc) tail
        | [] -> acc
    loop [] list

printfn "Inverted list: %A" (reverseList list)