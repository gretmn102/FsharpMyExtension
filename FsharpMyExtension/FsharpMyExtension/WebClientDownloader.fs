module FsharpMyExtension.WebClientDownloader
open FsharpMyExtension
open FsharpMyExtension.Either
open System.Net

let cookieContainer = CookieContainer()

ServicePointManager.DefaultConnectionLimit <- System.Int32.MaxValue

type CookieAwareWebClient() =
    inherit WebClient()

    override __.GetWebRequest(address) =
        match base.GetWebRequest address with
        | :? HttpWebRequest as req ->
            req.Timeout <- 2000
            req.CookieContainer <- cookieContainer
            req.AutomaticDecompression <- DecompressionMethods.GZip ||| DecompressionMethods.Deflate
            req :> WebRequest
        | req ->
            req

    // override __.GetWebResponse(request) =
    //     let response = base.GetWebResponse request
    //     match response.Headers.[HttpResponseHeader.SetCookie] with
    //     | null -> response
    //     | setCookieHeader ->
    //         printfn "setCookieHeader = %A" setCookieHeader
    //         let cookie = new Cookie()
    //         cookieContainer.Add(cookie)
    //         response

let webClient = new CookieAwareWebClient()

let post (headers:(string * string) list) (url:string) data =
    let hd = webClient.Headers

    headers |> List.iter hd.Set
    try
        webClient.UploadString(url, data)
        |> Right
    with
        | x -> Left x.Message

let postData (headers:(string * string) list) contentType (url:string) data =
    let hd = webClient.Headers
    headers |> List.iter hd.Set
    hd.Set(System.Net.HttpRequestHeader.ContentType, contentType)

    try
        webClient.UploadData(url, data)
        |> Right
    with
        | x -> Left x.Message

let get (headers:(string * string) list) (url:string) =
    let hd = webClient.Headers

    headers |> List.iter hd.Set
    try
        webClient.DownloadString url
        |> Right
    with
        | x -> Left x.Message

let put (headers:(string * string) list) (url:string) =
    let hd = webClient.Headers

    headers |> List.iter hd.Set
    try
        let data =
            webClient.UploadString(url, "PUT", "")

        Right data
    with
        | x -> Left x.Message

let patch (headers:(string * string) list) (url:string) (data:string) =
    let hd = webClient.Headers

    headers |> List.iter hd.Set
    try
        webClient.UploadString(url, "PATCH", data)
        |> Right
    with
        | x -> Left x.Message