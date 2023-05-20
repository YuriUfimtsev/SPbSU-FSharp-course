module Program

open System
open System.IO
open PhoneBook

let printInfo =
    printfn "Below you can find the available commands:"
    printfn "0 - exit;"
    printfn "1 - make a new entry (enter the name and phone number);"
    printfn "2 - find the phone number(s) of person (enter the person's name);"
    printfn "3 - find a person's name by phone number (enter the person's phone number);"
    printfn "4 - fill in the database from the file (enter the path to the file);"
    printfn "5 - save all entries in file (enter the path to the file);"
    printfn "6 - show all database contents;"

let readFromFile (path : string) =
    use streamReader = new StreamReader(path)
    streamReader.ReadToEnd() |> fillDatabaseFromString

let saveInFile (path : string) database =
    use streamWriter = new StreamWriter(path)
    let resultString = writeDatabaseToString database
    streamWriter.Write(resultString)

let printDatabase database =
    let rec loop localDatabase =
        match localDatabase with
        | head :: tail ->
            printfn "Name:%s; Phone:%s;" head.Name head.Phone
            loop tail
        | [] -> ()

    loop database

let printPhoneNumbers phones =
    let rec loop localPhones =
        match localPhones with
        | head :: tail ->
            printfn "%s, " head
            loop tail
        | [] -> ()

    match List.length phones with
    | 0 -> printfn "There is no such record in the database"
    | _ ->
        printf "Phones: "
        loop phones

let rec loop database =
    printfn "Enter the next command:"

    try
        let command = Console.ReadLine() |> int

        match command with
        | 0 -> ()
        | 1 ->
            printfn "Enter the name: "
            let name = Console.ReadLine()
            printfn "Enter the phone: "
            let phone = Console.ReadLine()
            let person = { Name = name; Phone = phone }
            addRecord person database |> loop
        | 2 ->
            findPhones (Console.ReadLine()) database |> printPhoneNumbers
            loop database
        | 3 ->
            let name = findName (Console.ReadLine()) database

            match name with
            | Some value -> printfn "Name: %s" value
            | None -> printfn "There is no such record in the database"

            loop database
        | 4 -> Console.ReadLine() |> readFromFile |> loop
        | 5 ->
            saveInFile (Console.ReadLine()) database
            loop database
        | 6 ->
            printDatabase database
            loop database
        | _ ->
            printfn "Incorrect command. Try again"
            loop database
    with :? FormatException ->
        printfn "Incorrect command. Try again"
        loop database

printfn "Welcome to the phone book!"
printInfo
loop []
