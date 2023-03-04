exception InvalidArgument of int

printfn "Enter the number"
let inputData = System.Console.ReadLine()
let number = int inputData

let factorial number =
    let rec loop i acc = 
        if i = 0 then acc
        else loop (i - 1) (i * acc)
    if number < 0 then raise (InvalidArgument(number))
    loop number 1

try
    printfn "Result: %d" (factorial number)
with
    | InvalidArgument(number) -> printfn "Incorrect argument: %d" number