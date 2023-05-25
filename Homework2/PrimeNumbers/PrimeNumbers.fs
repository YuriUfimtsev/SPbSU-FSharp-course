namespace PrimeNumbers

module PrimeNumbers =
    open System

    let primeNumberSequence () =
        let isPrimeNumber number =
            if number = bigint 0 then false
            else
                let checkLimit = number |> float32 |> MathF.Sqrt |> MathF.Round |> bigint |> (+) (bigint 1)
                let rec loop i =
                    if i = checkLimit + bigint 1 then true
                    else if number % i = bigint 0 && i <> number && i <> bigint 1 then false
                    else loop (i + bigint 1)
                loop (bigint 1)
        seq {
            yield! Seq.filter (fun number -> isPrimeNumber number) (Seq.initInfinite (fun i -> bigint i))
        }