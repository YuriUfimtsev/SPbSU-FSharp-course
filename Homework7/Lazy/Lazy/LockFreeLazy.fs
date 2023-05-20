namespace Lazy

open System.Threading

type LockFreeLazy<'a>(supplier : unit -> 'a) =
    let mutable privateSupplier : unit -> 'a = supplier

    let mutable result : Option<'a> = None

    interface ILazy<'a> with
        override this.Get () =
            match result with
            | None ->
                let value = privateSupplier ()
                Interlocked.CompareExchange(&result, Some value, None) |> ignore
                result.Value
            | Some value -> value
