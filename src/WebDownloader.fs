module FsharpMyExtension.WebDownloader
open System.Net
open FsharpMyExtension.Either

let cookies = CookieContainer()
// А еще есть `System.Net.Http`, который понадежнее будет всей моей писанины.

let defaultEncoding = System.Text.Encoding.UTF8
ServicePointManager.DefaultConnectionLimit <- System.Int32.MaxValue

type Url = string

module Decryption =
    let gzip (encoding:System.Text.Encoding) (receiveStream:System.IO.Stream) =
        try
            use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
            use r = new System.IO.StreamReader(zl, encoding)
            let str = r.ReadToEnd()
            Right str
        with e -> Left e
let createHttpReq (url:string) =
    // let url = "https://google.com/someimage.gif?query=20"
    let uri = System.Uri url
    let req = System.Net.WebRequest.CreateHttp(uri)
    req.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    match Path.getExtension uri.LocalPath with
    | ".webp" ->
        req.Accept <- "video/webm,video/ogg,video/*;q=0.9,application/ogg;q=0.7,audio/*;q=0.6,*/*;q=0.5"
    | ".mp4" ->
        req.Accept <- "video/mp4,video/ogg,video/*;q=0.9,application/ogg;q=0.7,audio/*;q=0.6,*/*;q=0.5"
    | ".gif" ->
        req.Accept <- "image/gif"
    | _ ->
        req.Accept <- "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        req.Headers.Add (System.Net.HttpRequestHeader.AcceptLanguage, "ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3")
    req.Referer <- sprintf "%s://%s" uri.Scheme uri.Host
    req.CookieContainer <- cookies
    req.Timeout <- 2500
    req

open FsharpMyExtension.Net
open FsharpMyExtension.Net.ContentType

type Content = Text of string | Binary of byte []

let defineEncoding contentType =
    match contentType with
    | null -> defaultEncoding
    | contentType ->
        let res = ContentType.Parser.start contentType
        match res with
        | Right x ->
            match x.Parameter with
            | Some (ContentType.Charset enc) -> enc
            | None -> defaultEncoding
            | Some _ -> defaultEncoding
        | Left x ->
            printfn "ContentTypeParser error:\n%A" x
            defaultEncoding
type ExnType =
    | WebException of System.Net.WebException
    | AnotherException of exn
    | NotFound
    | Forbidden
    | BadRequest
    | NameResolutionFailure
    | PostException of exn
    | ContentTypeParserError of string
[<Literal>]
let TooManyRequests : System.Net.HttpStatusCode = enum 429
let waitBeforeTrying = 2000
let waitWhenTooManyRequests = 10000
let countAttempts = 20

type ContentDisposition =
    // /// ```
    // /// attachment
    // /// attachment; filename="filename.jpg"
    // /// ```
    // | Attachment of string option
    // | Inline
    // | ParserErr of raw:string * errMsg:string
    Either<{|Raw:string; Err:string|}, Mime.ContentDisposition>

let contentDispositionParse str : ContentDisposition =
    try
        Right (Mime.ContentDisposition(str))
    with
        e -> Left {| Raw = str; Err = e.Message |}

type ReturnStatus =
    {
        StatusCode : System.Net.HttpStatusCode
        Location : string option
        ContentType : ContentType.ContentType option
        Content : Content
        ContentDisposition:ContentDisposition
    }

let getResp reqf url =
    let rec getResp attemptsNum =
        async {
            let! res = reqf (createHttpReq url)
            match res with
            | Right (req : System.Net.HttpWebRequest) ->
                // Reflection.Reflection.printProperties req.CookieContainer |> printfn "%A"
                try
                    let! resp = req.AsyncGetResponse()
                    return Right (resp :?> System.Net.HttpWebResponse)
                with
                    | :? System.Net.WebException as e ->
                        if e.Status = System.Net.WebExceptionStatus.ProtocolError then
                            use resp = e.Response :?> System.Net.HttpWebResponse
                            match resp.StatusCode with
                            | System.Net.HttpStatusCode.NotFound ->
                                return Left(NotFound)
                            | System.Net.HttpStatusCode.Forbidden ->
                                return Left(Forbidden)
                            | TooManyRequests ->
                                printfn "%s" e.Message
                                if attemptsNum < countAttempts then
                                    System.Threading.Thread.Sleep waitWhenTooManyRequests
                                    printfn "trying again..."
                                    return! getResp (attemptsNum + 1)
                                else
                                    return Left(WebException e)
                            | System.Net.HttpStatusCode.BadRequest ->
                                return Left(BadRequest)
                            | _ ->
                                printfn "%s" e.Message
                                if attemptsNum < countAttempts then
                                    System.Threading.Thread.Sleep waitBeforeTrying
                                    printfn "trying again..."
                                    return! getResp (attemptsNum + 1)
                                else
                                    return Left(WebException e)
                        elif e.Status = System.Net.WebExceptionStatus.Timeout then
                            printfn "%s" e.Message
                            if attemptsNum < countAttempts then
                                System.Threading.Thread.Sleep waitBeforeTrying
                                printfn "trying again..."
                                return! getResp (attemptsNum + 1)
                            else
                                return Left(WebException e)
                        elif e.Status = System.Net.WebExceptionStatus.NameResolutionFailure then // случается, когда нет интернета
                            return Left(NameResolutionFailure)
                        else
                            return Left(WebException e)
                    | e -> return Left(AnotherException e)
            | Left x -> return Left x
        }
    getResp 0


/// Если выбьет: дескать, не удалось создать защищенный канал SSL/TLS, — то:
/// ```fsharp
/// System.Net.ServicePointManager.SecurityProtocol <- System.Net.SecurityProtocolType.Tls12
/// ```
let tryGet2 reqf (url:string) =
    async {
        let! resp =
            url
            |> getResp (fun req ->
                async {
                    let! req = reqf req
                    match req with
                    | Left x -> return Left x
                    | Right (req : System.Net.HttpWebRequest) ->
                        // https://developer.mozilla.org/ru/docs/Web/HTTP/Headers/Accept-Encoding#Directives
                        req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
                        return Right req
                }
            )
        match resp with
        | Left x -> return Left x
        | Right resp ->
            use resp = resp
            let just () =
                match resp.Headers.["Content-Encoding"] with
                | null -> ()
                | x ->
                    // TODO: действительно архивируют картинки, например, в https://www.instagram.com/static/images/homepage/screenshot2.jpg/6f03eb85463c.jpg на 12.10.2020 14:34:40 было
                    // TODO: https://upload.wikimedia.org/wikipedia/commons/d/d6/URI_syntax_diagram.svg
                    printfn "`Content-Encoding:%s` бывает" x
                try
                    use out = resp.GetResponseStream()
                    use m = new System.IO.MemoryStream()
                    out.CopyTo m
                    Right( Binary(m.ToArray()) )
                with e -> Left(AnotherException e)
            let fn (encoding:System.Text.Encoding) =
                match resp.Headers.["Content-Encoding"] with
                | "gzip" ->
                    try
                        use receiveStream = resp.GetResponseStream()
                        use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
                        use r = new System.IO.StreamReader(zl, encoding)
                        let str = r.ReadToEnd()
                        Right(Text str)
                    with e ->
                        Left(AnotherException e)
                | _ ->
                    try
                        use stream = resp.GetResponseStream()
                        use reader = new System.IO.StreamReader(stream, encoding)
                        Right(Text (reader.ReadToEnd()))
                    with e ->
                        Left(AnotherException e)
            match resp.ContentType with
            | null
            | "" ->
                let res =
                    just ()
                    |> Either.map (fun content ->
                        {
                            StatusCode = resp.StatusCode
                            Location = resp.Headers.["Location"] |> Option.ofObj
                            ContentType = None
                            Content = content
                            ContentDisposition =
                                contentDispositionParse (resp.Headers.["Content-Disposition"])
                        }
                    )
                return res
            | contentType ->
                let res = ContentType.Parser.start contentType
                match res with
                | Right contentType ->
                    let getBinary() =
                        just ()
                        |> Either.map (fun content ->
                            {
                                StatusCode = resp.StatusCode
                                Location = resp.Headers.["Location"] |> Option.ofObj
                                ContentType = Some contentType
                                Content = content
                                ContentDisposition =
                                    contentDispositionParse (resp.Headers.["Content-Disposition"])
                            }
                        )
                    match contentType.Parameter with
                    | Some (ContentType.Charset enc) ->
                        let res =
                            fn enc
                            |> Either.map (fun content ->
                                {
                                    StatusCode = resp.StatusCode
                                    Location = resp.Headers.["Location"] |> Option.ofObj
                                    ContentType = Some contentType
                                    Content = content
                                    ContentDisposition =
                                        contentDispositionParse (resp.Headers.["Content-Disposition"])
                                }
                            )
                        return res
                    | None | Some _ ->
                        let getText() =
                            fn defaultEncoding
                            |> Either.map (fun content ->
                                {
                                    StatusCode = resp.StatusCode
                                    Location = resp.Headers.["Location"] |> Option.ofObj
                                    ContentType = Some contentType
                                    Content = content
                                    ContentDisposition =
                                        contentDispositionParse (resp.Headers.["Content-Disposition"])
                                }
                            )
                        match contentType.Typ with
                        | ContentType.Application ->
                            // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
                            // TODO: define where is binary content and where is text
                            match contentType.Subtype with
                            | "vnd.rar"
                            | "x-rar" // but he is not here https://www.iana.org/assignments/media-types/media-types.xhtml
                            | "zip" ->
                                return getBinary ()
                            | _ ->
                                return getText()
                        | ContentType.Message
                        | ContentType.Text ->
                            return getText()
                        | _ ->
                            return getBinary ()
                | Left x ->
                    return Left (ContentTypeParserError x)
    }

let tryGet reqf (url:string) =
    tryGet2 (reqf >> (async.Return << Right)) url

/// CookieContainer небезопасно использовать в параллельных вычислениях. Что делать? Включил, но использовать на свой страх и риск.
let getAsync reqf (urls: Url list) =
    let get url =
        let req : HttpWebRequest = reqf (createHttpReq url)

        req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
        async {
            try
                let! resp = req.AsyncGetResponse()
                use resp = resp :?> HttpWebResponse
                let encoding = defineEncoding resp.ContentType
                match resp.Headers.["Content-Encoding"] with
                | "gzip" ->
                    try
                        use receiveStream = resp.GetResponseStream()
                        use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
                        use r = new System.IO.StreamReader(zl, encoding)
                        let str = r.ReadToEnd()
                        return (url, Right(resp.StatusCode, str))
                    with e -> return url, Left e
                | _ -> // `null` возьмет?
                    use stream = resp.GetResponseStream()
                    use reader = new System.IO.StreamReader(stream, encoding)
                    return (url, Right(resp.StatusCode, reader.ReadToEnd()))
            with e -> return (url, Left e)
        }
    let webPages = List.map get >> Async.Parallel >> Async.RunSynchronously
    List.chunkBySize 8 urls
    |> Seq.collect webPages

[<System.ObsoleteAttribute("`postData` `url` -> `url` `postData`\nНе знал, каким еще способом это сказать", false)>]
let tryPost reqf (url:string) (postData:string) =
    url
    |> tryGet2 (reqf >> fun (req:System.Net.HttpWebRequest) ->
        req.Method <- "POST"
        req.ContentType <- "application/x-www-form-urlencoded"

        let data = System.Text.Encoding.ASCII.GetBytes(postData)
        req.ContentLength <- int64 data.Length

        // https://developer.mozilla.org/ru/docs/Web/HTTP/Headers/Accept-Encoding#Directives
        req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
        async {
            try
                use stream = req.GetRequestStream()
                stream.Write(data, 0, data.Length)

                return Right req
            with e -> return Left (PostException e)
        }
    )

(*
/// test
let mainUrl = @"http://www.google.com"
let enterUrl () =
    let main = mainUrl |> downloadWebpage |> XmlAnalyzer.htmlToXml
    let node = main.SelectSingleNode "//*[@id='enter_link']"
    let href = node.SelectSingleNode "./@href"
    let url = mainUrl + href.Value
    url |> downloadWebpage |> ignore
enterUrl ()

let urls =
    File.ReadLines @"picpages.txt"
    |> Seq.truncate 10 |> List.ofSeq
let fileNames = urls |> List.map (fun x -> let uri = UrlNew x in uri.Segments.[uri.Segments.Length - 1]) |> List.map (fun x -> x + ".html")
let pages = urls |> getAsync
List.zip fileNames pages |> List.iter (File.WriteAllText)
*)

// open System.Web;

// let getToFile (url:string) path rawPath =
//     if not rawPath && path <> "" then
//         Directory.CreateDirectory(path) |> ignore
//     let webReq = System.Net.WebRequest.CreateHttp(url)

//     //webReq.CookieContainer = cookie;

//     use response = webReq.GetResponse()
//     use stream = response.GetResponseStream()

//     /// <summary>
//     /// Copies the contents of input to output. Doesn't close either stream.
//     /// </summary>
//     let streamToStream ((input:Stream), (output:Stream)) =
//         let buffer = Array.create (8 * 1024) (byte 0)
//         let rec f () =
//             let len = input.Read(buffer, 0, buffer.Length)
//             if len > 0 then
//                 output.Write(buffer, 0, len)
//                 f()
//         f()

//     let path =
//         if rawPath then path
//         else
//             let segments = response.ResponseUri.Segments;
//             let segmentLast = segments.[segments.Length - 1];
//             let segmentLast = HttpUtility.UrlDecode(segmentLast);
//             path + segmentLast

//     using (File.Create(path))
//         (fun x -> streamToStream(stream, x))


module DownThemAll =
    open FsharpMyExtension.XmlBuilder
    // let f () =
    //     System.IO.File.ReadAllText @"e:\downloads\Downloads.meta4"
    //     |> FsharpMyExtension.HtmlNode.HtmlNode.ofString
    //     |> fun x -> x.ChildNodes
    //     |> Node.ofHtmlNodes
    //     |> Seq.map (sprintf "%A")
    //     |> fun cont -> System.IO.File.WriteAllLines("output/outputTemp.txt", cont)
    /// DownThemAll! 3.0v
    /// (path * url) list
    let metalink (xs:(string * string) seq) =
        let now = System.DateTime.Now
        let nowStr =
            now
            |> FsharpMyExtension.DateTime.Unix.toMSec
            |> string
        // let now = System.DateTime.Now
        // now.ToString("R")

        let file i (name, url) =
            Node
              ("file",
               [("name", name)
                ("a0:num", sprintf "%d" i)
                ("a0:startdate", nowStr) // "1524916470084");
                ("a0:referrer", "")],
               [
                //Node ("description",[],[Text ""]);
                Node
                  ("url",
                   [("priority", "100");
                    ("a0:usable", url)],
                   [TextStr url])
                ]);
        Node
          ("metalink",
           [("xmlns", "urn:ietf:params:xml:ns:metalink"); ("version", "4.0");
            ("a0:version", "3.0.8");
            ("xmlns:a0", "http://www.downthemall.net/properties#")],
           [yield Node ("generator",[],[Text "DownThemAll!/3.0"]);
            yield Node ("published",[],[Text (now.ToString("R"))]) //"Sat, 28 Apr 2018 11:55:16 GMT"]);
            yield! Seq.mapi file xs
           ])
        |> Node.sprintNodeXml
    /// filepath -> (path * url) list
    let metalinkf (path:string) (xs:(string * string) list) =
        metalink xs |> fun cont -> System.IO.File.WriteAllText(path, cont)
