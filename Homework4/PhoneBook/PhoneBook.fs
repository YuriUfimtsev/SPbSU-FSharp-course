module PhoneBook

open System.IO

type Person = { Name : string; Phone : string }

let addRecord person database =
    let isAdditionCorrect = List.exists (fun x -> x.Phone = person.Phone) database |> not
    if isAdditionCorrect then person :: database else database

let findName phone database =
    let person = List.tryFind (fun x -> phone = x.Phone) database
    match person with
    | Some value -> Some value.Name
    | None -> None

let findPhones name database =
    database |> List.filter (fun x -> name = x.Name) |> List.map (fun x -> x.Phone)

let saveInFile (path : string) database =
    use streamWriter = new StreamWriter(path)

    let rec loop i localDatabase =
        if i >= List.length database then
            ()
        else
            match localDatabase with
            | head :: tail ->
                streamWriter.WriteLine(
                    String.concat
                        " "
                        (seq {
                            head.Name
                            head.Phone
                        })
                )

                loop (i + 1) tail
            | [] -> ()

    loop 0 database

let getFromFile (path : string) =
    use streamReader = new StreamReader(path)

    let rec loop (newLine : string) database =
        if newLine = null then
            database
        else
            let array = newLine.Split ' '
            if Array.length array <> 2 then
                raise (invalidArg "Name and phone" "More than two parameters were found")
            let newRecord = { Name = array[0]; Phone = array[1] }
            loop (streamReader.ReadLine()) (newRecord :: database)

    loop (streamReader.ReadLine()) []