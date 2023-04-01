namespace BracketsBalance

module Stack =
    let push stack element = element :: stack

    let pop stack =
        match stack with
        | head :: tail -> tail
        | [] -> []

    let top stack =
        match stack with
        | head :: tail -> Some(head)
        | [] -> None

    let isEmpty stack =
        match stack with
        | head :: tail -> false
        | [] -> true
