module MiniCrawler

open System
open FSharp.Data
open System.Net.Http

let getWebPagesInfo (url : string) =
    async {
        try
            let mainWebPage = HtmlDocument.Load(url)

            let links =
                mainWebPage.Descendants [ "a" ]
                |> Seq.choose (fun x -> x.TryGetAttribute("href") |> Option.map (fun attribute -> attribute.Value()))

            let client = new HttpClient()

            let tasksArray =
                Seq.map
                    (fun (link : string) ->
                        async {
                            try
                                let! result = Async.AwaitTask(client.GetStringAsync(link))
                                return Some(link, result.Length)
                            with _ ->
                                return None
                        })
                    links

            return Some(tasksArray |> Async.Parallel)
        with :? Net.WebException ->
            raise (ArgumentException "Incorrect url")
            return None
    }
