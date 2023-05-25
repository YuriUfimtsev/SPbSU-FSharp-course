module MiniCrawlerTests

open System
open NUnit.Framework
open FsUnit
open MiniCrawler

[<Test>]
let StandartTest () =
    getWebPagesInfo "https://se.math.spbu.ru/"
    |> Async.RunSynchronously
    |> Seq.filter (fun linkInfo -> linkInfo <> None)
    |> Seq.map (fun linkInfo -> linkInfo.Value)

    |> Seq.toList
    |> should
        equal
        [ ("https://spbu.ru/", 98486);
          ("https://oops.math.spbu.ru/SE/alumni", 49175);
          ("https://ru.wikipedia.org/wiki/%D0%A2%D0%B5%D1%80%D0%B5%D1%85%D0%BE%D0%B2,"
           + "_%D0%90%D0%BD%D0%B4%D1%80%D0%B5%D0%B9_%D0%9D%D0%B8%D0%BA%D0%BE%D0%BB%D0%B0%D0%B5%D0%B2%D0%B8%D1%87",
           90706);
          ("https://www.acm.org/binaries/content/assets/education/curricula-recommendations/cc2005-march06final.pdf",
           758040);
          ("https://t.me/sysprog2020admission", 9729);
          ("https://oops.math.spbu.ru/SE", 23408);
          ("https://oops.math.spbu.ru/SE/alumni", 49175); ]

[<Test>]
let TestWithIncorrectUrl () =
    (fun () -> getWebPagesInfo "https://se.ma.su.ru/" |> ignore)
    |> should throw typeof<ArgumentException>