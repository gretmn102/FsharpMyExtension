module FsharpMyExtension.WebDownloader
open System
open System.Net
open System.IO
open Microsoft.FSharp.Control.WebExtensions
open FsharpMyExtension.Either

let cookies = CookieContainer()
ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

// код ради Uri "http://x/y." ибо в оригинальном Uri преобразует его в "http://x/y"
do
    let getSyntax = typeof<UriParser>.GetMethod("GetSyntax", System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.NonPublic);
    let flagsField = typeof<UriParser>.GetField("m_Flags", System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.NonPublic);
    if (isNull getSyntax && isNull flagsField) then
        [ "http"; "https" ]
        |> List.iter (fun scheme ->
            let parser = getSyntax.Invoke(null, [| scheme |]) :?> UriParser
            if (isNull parser) then
                let flagsValue = flagsField.GetValue(parser) :?> int
                // Clear the CanonicalizeAsFilePath attribute
                if ((flagsValue &&& 0x1000000) <> 0) then
                    flagsField.SetValue(parser, flagsValue &&& ~~~0x1000000);
        )
        
type Url = string
type Content = string
//type Result = 
//    | Success of Url * HttpStatusCode * string
//    | Fail of Url * Exception
type Res = Either<(Url*Exception), (Url * HttpStatusCode * Content)>

/// <summary> Использует Cookies, Uri поддерживающий точку </summary>
let get encoding =
    fun (url:string) ->
        let req = WebRequest.Create(url) :?> HttpWebRequest
        req.CookieContainer <- cookies
        use resp = req.GetResponse()
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream, (encoding:Text.Encoding)) //Text.Encoding.GetEncoding "windows-1251"
        reader.ReadToEnd()

let tryGet encoding (url:string) =
    let req = WebRequest.Create(url) :?> HttpWebRequest
    req.CookieContainer <- cookies
    try
        use resp = req.GetResponse() :?> HttpWebResponse
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream, (encoding:Text.Encoding)) //Text.Encoding.GetEncoding "windows-1251"
        Right(url, resp.StatusCode, reader.ReadToEnd()) : Res
    with e -> Left(url, e)

// assert
//     let c = CookieContainer()
//     let uri = Uri("https://www.google.com")
//     c.SetCookies(uri, "PHPSESSID=1")
//     c.GetCookieHeader(uri)
//     let s f = lock c f
//     let xs = Seq.init 10000 (fun i -> async { c.SetCookies(uri, sprintf "PHPSESSID=%d" i) })
//     Async.Parallel xs |> Async.RunSynchronously |> ignore
//     c.GetCookieHeader uri
    
    
//     let monitor = Object()
//     let a = ref 4

//     printfn "1) a = %i" !a

//     let t1 = System.Threading.Thread (fun () ->  
//         printfn "locked in thread 1"    
//         lock monitor (fun () -> a:= !a + 2)
//         printfn "unlocked in thread 1"    
//         )

//     let t2 = System.Threading.Thread (fun () ->  
//         printfn "locked in thread 2"    
//         lock monitor (fun () -> a:= !a - 3)
//         printfn "unlocked in thread 2"    
//         )

//     t1.Start()
//     t2.Start()

//     System.Threading.Thread.Sleep 1000 // wait long enough to get the correct value
//     printfn "2) a = %i" !a
    
//     true

/// CookieContainer небезопастно использовать в паралельных вычислениях. Что делать? Включил, но использовать на свой страх и риск.
let getAsync encoding urls =
    let get url = 
        let req = WebRequest.Create(url:string) :?> HttpWebRequest
        req.CookieContainer <- cookies
        async {
            try
                let! rsp = req.AsyncGetResponse()
                let rsp = rsp :?> HttpWebResponse
                
                use stream = rsp.GetResponseStream()
                use reader = new StreamReader(stream, (encoding:Text.Encoding)) //Text.Encoding.GetEncoding "windows-1251"
                let r = reader.ReadToEnd() // .AsyncReadToEnd()
                return (Right(url, rsp.StatusCode, r) : Res)
            with
            | ex -> return (Left(url, ex) : Res)
        }
    let webPages = List.map get >> Async.Parallel >> Async.RunSynchronously
    List.chunkBySize 8 urls
    |> Seq.collect webPages

let post (url:string) (postData:string) = 
    let request = WebRequest.Create(url) :?> HttpWebRequest

    request.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    request.CookieContainer <- cookies;
    request.Method <- "POST"
    request.ContentType <- "application/x-www-form-urlencoded"

    let data = Text.Encoding.ASCII.GetBytes(postData)
    request.ContentLength <- int64 data.Length

    using (request.GetRequestStream())
            (fun stream -> stream.Write(data, 0, data.Length))

    use resp = request.GetResponse() :?> HttpWebResponse
       
    use stream = resp.GetResponseStream()
    use reader = new StreamReader(stream, Text.Encoding.GetEncoding "windows-1251")
    reader.ReadToEnd()

let tryPost (url:string) (postData:string) = 
    let request = WebRequest.Create(url) :?> HttpWebRequest

    request.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    request.CookieContainer <- cookies;
    request.Method <- "POST"
    request.ContentType <- "application/x-www-form-urlencoded"

    let data = Text.Encoding.ASCII.GetBytes(postData)
    request.ContentLength <- int64 data.Length

    try 
        using (request.GetRequestStream())
            (fun stream -> stream.Write(data, 0, data.Length))
        use resp = request.GetResponse() :?> HttpWebResponse
       
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream, Text.Encoding.GetEncoding "windows-1251")
        Right(url, resp.StatusCode, reader.ReadToEnd()) : Res
    with e -> Left(url, e)

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
//     let webReq = WebRequest.Create(url) :?> HttpWebRequest

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


