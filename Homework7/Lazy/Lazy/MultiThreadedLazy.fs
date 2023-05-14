namespace Lazy

type MultiThreadedLazy<'a>(supplier : unit -> 'a) =
    let mutable privateSupplier : Option<unit -> 'a> = Some supplier

    [<VolatileField>]
    let mutable result : Option<'a> = None

    let synchronizationObject = obj

    interface ILazy<'a> with
        override _.Get () =
            match result with
            | None ->
                lock synchronizationObject (fun () ->
                    match result with
                    | None ->
                        let supplier = privateSupplier |> Option.get
                        let value = supplier ()
                        privateSupplier <- None
                        result <- Some value
                        value
                    | Some value -> value)
            | Some value -> value
