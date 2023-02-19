printfn "Enter the number"
let data = System.Console.ReadLine()
let number = int data

let factorial number =
    let rec support x acc = 
        if x = 0 then acc
        else support (x - 1) (x * acc)
    support number 1

printfn "Result: %d" (factorial number)
