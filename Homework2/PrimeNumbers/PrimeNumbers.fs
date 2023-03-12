open System

let primeNumberSequence () =
    let isPrimeNumber number =
        let checkLimit = number |> float32 |> MathF.Sqrt |> MathF.Round |> bigint |> (+) (bigint 1)
        let rec loop i =
            if i = checkLimit + bigint 1 then true
            else if number % i = bigint 0 && i <> number && i <> bigint 1 then false
            else loop (i + bigint 1)
        loop (bigint 1)
    bigint 1
    |> Seq.unfold (fun state ->
    if isPrimeNumber state then Some(state, state + bigint 1)
    else Some(bigint 0, state + bigint 1))
    |> Seq.filter (fun number ->
        if number <> bigint 0 then true
        else false)