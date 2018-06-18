module FsharpMyExtension.Json
open Newtonsoft.Json
// open System.Xml
let ser x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Formatting.Indented)
let serf path x = ser x |> fun x -> System.IO.File.WriteAllText(path, x)
let serNotIndent x = Newtonsoft.Json.JsonConvert.SerializeObject x
let serfNotIdent path x = serNotIndent x |> fun x -> System.IO.File.WriteAllText(path, x)
let des x = JsonConvert.DeserializeObject<_> x
let desf path = System.IO.File.ReadAllText path |> des

let bsonDesf path = 
    let txt = System.IO.File.OpenRead path
    use x = new Bson.BsonDataReader(txt)
    let json = JsonSerializer()
    // let s = json.Deserialize<_> x
    json.Deserialize<_> x
    // let txt = JsonConvert.SerializeObject(s, Formatting.Indented)
    // txt
// bson @"c:\Downloads\pascal\WorkshopUpload" |> fun x -> System.IO.File.WriteAllText(@"c:\Downloads\pascal\sabouter.json", x)
// desf @"c:\Downloads\pascal\red dragon inn.json" |> serf @"c:\Downloads\pascal\red dragon inn ident.json"