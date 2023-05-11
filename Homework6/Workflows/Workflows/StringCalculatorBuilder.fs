module StringCalculatorBuilder

open System

type StringCalculatorBuilder() =
    member _.Bind (value : string, func) =
        match Int32.TryParse value with
        | true, number -> func number
        | false, _ -> None

    member _.Return value = Some value
