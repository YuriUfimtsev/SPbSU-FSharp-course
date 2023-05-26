module MiniCrawlerTests

open System
open NUnit.Framework
open FsUnit
open MiniCrawler

[<Test>]
let StandartTest () =
    getWebPagesInfo "https://my.spbu.ru/"
    |> (Async.RunSynchronously)
    |> Option.get
    |> (Async.RunSynchronously)
    |> Seq.filter (fun linkInfo -> linkInfo <> None)
    |> Seq.map (fun linkInfo -> linkInfo.Value)

    |> Seq.toList
    |> should equal [ ("https://edu.spbu.ru/maps/map.html ", 106621) ]

[<Test>]
let TestWithIncorrectUrl () =
    (fun () -> getWebPagesInfo "https://se.ma.su.ru/" |> ignore)
    |> should throw typeof<ArgumentException>
