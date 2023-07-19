module PhoneBookTests

open System.IO
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
    findName "+79227149407" (database ())
    |> Option.defaultValue " "
    |> should equal "Vasily"

[<Test>]
let ``findName in database from "+00000000000" phone number should return None`` () =
    findName "+00000000000" (database ()) |> should equal None

[<Test>]
let ``findName in empty database from "+79227149407" phone number should return None`` () =
    findName "+79227149407" (emptyDatabase ()) |> should equal None

[<Test>]
let ``findPhones in database from "Ivan" name should return list of "+78000000000"`` () =
    findPhones "Ivan" (database ()) |> should equal [ "+78000000000" ]

[<Test>]
let ``findPhones in database from "Andrey" name should return empty list`` () =
    findPhones "Andrey" (database ()) |> should be Empty

[<Test>]
let ``findPhones in empty database from "Ivan" name should return empty list`` () =
    findPhones "Ivan" (emptyDatabase ()) |> should be Empty

[<Test>]
let ``Add unique record in database should return new database`` () =
    let record =
        { Name = "Yuri"
          Phone = "+678768769769" }

    let newDatabase = addRecord record (database ())

    List.sort newDatabase
    |> should
        equal
        (List.sort
            [ { Name = "Vasily"
                Phone = "+79227149407" }
              { Name = "Ivan"
                Phone = "+78000000000" }
              { Name = "Petr"
                Phone = "+79347608556" }
              { Name = "Yuri"
                Phone = "+678768769769" } ])

[<Test>]
let ``Add record with already existing phone number in database should return unmodified database`` () =
    let record =
        { Name = "Yuri"
          Phone = "+78000000000" }

    addRecord record (database ()) |> should equal (database ())

[<Test>]
let ``Get database from not empty string should return not empty database`` () =
    let data = "Anton +100\nIvan +200\nSergey +300"
    let database = fillDatabaseFromString data

    let expectedDatabase =
        [ { Name = "Anton"; Phone = "+100" }
          { Name = "Ivan"; Phone = "+200" }
          { Name = "Sergey"; Phone = "+300" } ]

    List.sort database |> should equal (List.sort expectedDatabase)

[<Test>]
let ``Get database from empty string should return empty database`` () =
    let data = ""
    let database = fillDatabaseFromString data
    database |> should be Empty

[<Test>]
let ``Write database to string test`` () =
    let actualResult = writeDatabaseToString (database ())
    let expectedResult = "Vasily +79227149407\nIvan +78000000000\nPetr +79347608556"
    actualResult |> should equal expectedResult
