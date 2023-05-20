module PhoneBook

type Person = { Name : string; Phone : string }

let addRecord person database =
    let isAdditionCorrect =
        List.exists (fun x -> x.Phone = person.Phone) database |> not

    if isAdditionCorrect then person :: database else database

let findName phone database =
    let person = List.tryFind (fun x -> phone = x.Phone) database

    match person with
    | Some value -> Some value.Name
    | None -> None

let findPhones name database =
    database |> List.filter (fun x -> name = x.Name) |> List.map (fun x -> x.Phone)

let fillDatabaseFromString (data : string) =
    let elements = data.Split("\n")

    let rec loop database i =
        if i >= elements.Length then
            database
        else
            let personInfoString = elements[i]
            let dataArray = personInfoString.Split ' '

            if Array.length dataArray <> 2 then
                raise (invalidArg "Name and phone" "More than two parameters were found")

            let newRecord =
                { Name = dataArray[0]
                  Phone = dataArray[1] }

            loop (newRecord :: database) (i + 1)

    if elements = [| "" |] then [] else loop [] 0

let writeDatabaseToString database =
    let rec loop localDatabase resultString =
        match localDatabase with
        | [] -> resultString
        | head :: tail ->
            loop
                tail
                (String.concat
                    "\n"
                    (seq {
                        resultString

                        (String.concat
                            " "
                            (seq {
                                head.Name
                                head.Phone
                            }))
                    }))

    (loop database "").Substring(1)
