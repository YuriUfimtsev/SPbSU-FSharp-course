printfn "Enter the number"
let inputData = System.Console.ReadLine()
let number = int inputData

let factorial number =
    let rec loop i acc = 
        if i = 0 then acc
        else loop (i - 1) (i * acc)
    loop number 1

if number >= 0 then printfn "Result: %d" (factorial number)
else printfn "Not defined"