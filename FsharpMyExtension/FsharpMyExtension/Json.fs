module Json
open Newtonsoft.Json

let ser x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Formatting.Indented)
let serf path x = ser x |> fun x -> System.IO.File.WriteAllText(path, x)
let serNotIndent x = Newtonsoft.Json.JsonConvert.SerializeObject x
let serfNotIdent path x = serNotIndent x |> fun x -> System.IO.File.WriteAllText(path, x)
let des x = Newtonsoft.Json.JsonConvert.DeserializeObject<_> x
let desf path = System.IO.File.ReadAllText path |> des