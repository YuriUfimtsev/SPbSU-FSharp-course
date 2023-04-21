namespace ControlWork1

module Stack =
    type Stack() =
        let mutable stack = []

        member this.Push element =
            stack <- (element :: stack)

        member this.TryPop =
            match stack with
            | head :: tail -> Some(head)
            | [] -> None