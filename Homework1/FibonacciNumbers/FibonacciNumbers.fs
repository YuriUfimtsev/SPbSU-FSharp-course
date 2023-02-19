let calculateFibonacciNumber numberInSequence =
    if numberInSequence <= 0 then 0
    elif numberInSequence = 1 then 1
    else
        let rec loop first second i =
            if i = numberInSequence then first
            else loop (second) (first + second) (i + 1)
        loop 0 1 0

printfn "The twentieth Fibonacci number: %d" (calculateFibonacciNumber 20)