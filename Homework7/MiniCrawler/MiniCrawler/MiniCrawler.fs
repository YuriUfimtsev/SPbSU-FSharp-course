module MiniCrawler

open System
open FSharp.Data
open System.Net.Http

let getWebPagesInfo (url : string) =
    try
        let mainWebPage = HtmlDocument.AsyncLoad(url)
        let parsingResul = mainWebPage |> Async.RunSynchronously

        let links =
            parsingResul.Descendants [ "a" ]
            |> Seq.choose (fun x -> x.TryGetAttribute("href") |> Option.map (fun attribute -> attribute.Value()))

        let client = new HttpClient()

        let tasksArray =
            Seq.map (fun (link : string) ->
                async {
                    try
                        let! result = Async.AwaitTask(client.GetStringAsync(link))
                        return Some (link, result.Length)
                    with _ ->
                        return None
                }) links

        tasksArray |> Async.Parallel
    with :? Net.WebException ->
        raise (ArgumentException "Incorrect url")