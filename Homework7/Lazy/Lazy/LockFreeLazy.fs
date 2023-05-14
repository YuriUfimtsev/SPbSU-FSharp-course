namespace Lazy

open System.Threading

type LockFreeLazy<'a> (supplier : unit -> 'a) =
    let mutable privateSupplier: Option<unit -> 'a> = Some supplier

    let mutable result: Option<'a> = None

    let rec get () =
        match result with
            | None ->
                    let currentResult = result
                    let supplier = privateSupplier |> Option.get
                    let value = supplier ()
                    if obj.ReferenceEquals
                        (result, Interlocked.CompareExchange(&result, Some value, currentResult)) |> not
                        then get ()
                        else result |> Option.get
            | Some value -> value

    interface ILazy<'a> with
        override this.Get () =
            get ()