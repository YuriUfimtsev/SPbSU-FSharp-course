namespace Lazy

type SingleThreadedLazy<'a> (supplier : unit -> 'a) =
    let mutable privateSupplier: Option<unit -> 'a> = Some supplier
    let mutable result: Option<'a> = None

    interface ILazy<'a> with
        override _.Get () =
            match result with
            | None ->
                let supplier = privateSupplier |> Option.get
                let value = supplier ()
                privateSupplier <- None
                result <- Some value
                value
            | Some value -> value