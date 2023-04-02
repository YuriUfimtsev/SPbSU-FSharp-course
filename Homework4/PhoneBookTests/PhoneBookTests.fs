module PhoneBookTests

open System.IO;
open FsUnit
open NUnit.Framework
open PhoneBook

let database () =
    [ { Name = "Vasily"
        Phone = "+79227149407" }
      { Name = "Ivan"
        Phone = "+78000000000" }
      { Name = "Petr"
        Phone = "+79347608556" } ]

let emptyDatabase () = []

[<Test>]
let ``findName in database from "+79227149407" phone number should return Some("Vasily")`` () =
    findName "+79227149407" (database ()) |> Option.defaultValue " " |> should equal "Vasily"

[<Test>]
let ``findName in database from "+00000000000" phone number should return None`` () =
    findName "+00000000000" (database ()) |> should equal None

[<Test>]
let ``findName in empty database from "+79227149407" phone number should return None`` () =
    findName "+79227149407" (emptyDatabase ()) |> should equal None

[<Test>]
let ``findPhones in database from "Ivan" name should return list of "+78000000000"`` () =
    findPhones "Ivan" (database ()) |> should equal ["+78000000000"]

[<Test>]
let ``findPhones in database from "Andrey" name should return empty list`` () =
    findPhones "Andrey" (database ()) |> should be Empty

[<Test>]
let ``findPhones in empty database from "Ivan" name should return empty list`` () =
    findPhones "Ivan" (emptyDatabase ()) |> should be Empty

[<Test>]
let ``Add unique record in database should return new database`` () =
    let record = { Name = "Yuri"; Phone = "+678768769769" }
    let newDatabase = addRecord record (database ())
    List.exists (fun x -> x = record) newDatabase |> should be True

[<Test>]
let ``Add record with already existing phone number in database should return unmodified database`` () =
    let record = { Name = "Yuri"; Phone = "+78000000000" }
    addRecord record (database ()) |> should equal (database ())

[<Test>]
let ``Get database from not empty file should return not empty database`` () =
    let pathToFile = "GetTestDatabase.txt"
    let database = getFromFile pathToFile
    let expectedDatabase =
        [
            { Name = "Anton"; Phone = "+100" }
            { Name = "Ivan"; Phone = "+200" }
            { Name = "Sergey"; Phone = "+300" }
        ]
    List.sort database |> should equal (List.sort expectedDatabase)

[<Test>]
let ``Get database from empty file should return empty database`` () =
    let pathToFile = "EmptyFile.txt"
    let database = getFromFile pathToFile
    database |> should be Empty

let isTwoFilesEquals (pathToFirstFile : string) (pathToSecondFile : string) =
    use firstStreamReader = new StreamReader(pathToFirstFile)
    use secondStreamReader = new StreamReader(pathToSecondFile)

    let rec loop firstFileLine secondFileLine =
        match firstFileLine, secondFileLine with
        | null, null -> true
        | null, _ -> false
        | _, null -> false
        | firstLine, secondLine when firstLine = secondLine ->
            loop (firstStreamReader.ReadLine()) (secondStreamReader.ReadLine())
        | _ -> false

    loop "" ""

[<Test>]
let ``Save database in file test`` () =
    let pathToGoldFile = "GoldDatabase.txt"
    let pathToTestFile = "SaveTestDatabase.txt"
    saveInFile pathToTestFile (database ())
    let comparisonResult = isTwoFilesEquals pathToTestFile pathToGoldFile
    if comparisonResult then File.Delete pathToTestFile
    comparisonResult |> should be True