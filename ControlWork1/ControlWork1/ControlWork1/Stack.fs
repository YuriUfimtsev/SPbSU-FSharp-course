namespace ControlWork1

module Stack =
    type Stack() =
        let mutable stack = []
        let lockObject = obj

        member this.Push element =
            lock lockObject (fun () -> stack <- (element :: stack))

        member this.TryPop =
            lock lockObject (fun () ->
            match stack with
            | head :: tail -> Some(head)
            | [] -> None)