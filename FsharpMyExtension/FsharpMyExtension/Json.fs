namespace FsharpMyExtension
[<RequireQualifiedAccess>]
module Json =
    open Newtonsoft.Json

    let ser x = Newtonsoft.Json.JsonConvert.SerializeObject(x, Formatting.Indented)
    let serf path x = ser x |> fun x -> System.IO.File.WriteAllText(path, x)
    let serNotIndent x = Newtonsoft.Json.JsonConvert.SerializeObject x
    let serfNotIdent path x = serNotIndent x |> fun x -> System.IO.File.WriteAllText(path, x)
    let des x = JsonConvert.DeserializeObject<_> x
    let desf path = System.IO.File.ReadAllText path |> des

[<RequireQualifiedAccess>]
module Bson =
    open Newtonsoft.Json

    let serialize v =
        use m = new System.IO.MemoryStream()
        use bwr = new Bson.BsonDataWriter(m)
        let ser = JsonSerializer()
        ser.Formatting <- Formatting.None
        ser.Serialize(bwr, v)
        m.ToArray()

    let deserializeBytes (bytes: byte []) =
        use m = new System.IO.MemoryStream(bytes)
        use x = new Bson.BsonDataReader(m)
        let json = JsonSerializer()
        json.Deserialize<_> x

    let deserializeFile path =
        let txt = System.IO.File.OpenRead path
        use x = new Bson.BsonDataReader(txt)
        let json = JsonSerializer()
        json.Deserialize<_> x

module JToken =
    let ofFile (path:string) =
        use st = System.IO.File.OpenText path
        use r = new Newtonsoft.Json.JsonTextReader(st)
        Newtonsoft.Json.Linq.JToken.ReadFrom(r)

    let toFile path (x:Newtonsoft.Json.Linq.JToken) =
        use file = System.IO.File.CreateText(path)
        use st = new Newtonsoft.Json.JsonTextWriter(file )
        st.Formatting <- Newtonsoft.Json.Formatting.Indented
        x.WriteTo(st)

module FSharpJsonType =
    open FsharpMyExtension
    open FsharpMyExtension.Reflection
    open Newtonsoft.Json.Linq

    type JsonFType =
        | BoolTyp     of bool
        | IntegerTyp  of int
        | StringTyp   of string
        | FloatTyp    of float
        | DateTimeTyp of System.DateTime
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module JsonFType =
        let getBool = function
            | BoolTyp x -> x
            | x -> failwithf "is not BoolTyp:\n%A" x
        let getInt = function
            | IntegerTyp x -> x
            | x -> failwithf "is not IntegerTyp:\n%A" x
        let getString = function
            | StringTyp x -> x
            | x -> failwithf "is not StringTyp:\n%A" x
        let getFloat = function
            | FloatTyp x -> x
            | x -> failwithf "is not FloatTyp:\n%A" x
    type JsonF =
        | Scalar    of JsonFType
        | Null
        | Obj       of Map<string, JsonF>
        | Sequence  of JsonF []
    // Вроде бы, так. Ужасно, что `Sequence` вмещает произвольные типы (вот же шл!..). Вот если бы она была бы одного типа, ах!
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module JsonF =
        let getObj = function
            | Obj x -> x
            | x -> failwithf "is not Obj:\n%A" x
        let getSeq = function
            | Sequence x -> x
            | x -> failwithf "is not Sequence:\n%A" x
        let getScalar = function
            | Scalar x -> x
            | x -> failwithf "is not Scalar:\n%A" x

        let rec ofJToken (x:JToken) =
            let t = x.Type
            match t with
            | JTokenType.Array ->
                x :?> JArray
                |> Seq.map ofJToken
                |> Array.ofSeq
                |> Sequence
            | JTokenType.Object ->
                x :?> JObject
                |> Seq.fold (fun st x ->
                    let x = x :?> JProperty
                    Map.add x.Name (ofJToken x.Value) st )
                    Map.empty
                |> Obj
            | JTokenType.Boolean -> Scalar(BoolTyp(x.ToObject()))
            | JTokenType.Integer -> Scalar(IntegerTyp(x.ToObject()))
            | JTokenType.String  -> Scalar(StringTyp(x.ToObject()))
            | JTokenType.Null    -> Null
            | JTokenType.Float   -> Scalar(FloatTyp(x.ToObject()))
            | JTokenType.Date    -> Scalar(DateTimeTyp(x.ToObject()))
            | t ->
                failwithf "unknown %A" (t, x.ToString())
            // | JTokenType.None -> failwith ""
            // | JTokenType.Constructor -> failwith ""
            // | JTokenType.Property -> failwith ""
            // | JTokenType.Comment -> failwith ""
            // | JTokenType.Undefined -> failwith ""
            // | JTokenType.Date -> failwith ""
            // | JTokenType.Raw -> failwith ""
            // | JTokenType.Bytes -> failwith ""
            // | JTokenType.Guid -> failwith ""
            // | JTokenType.Uri -> failwith ""
            // | JTokenType.TimeSpan -> failwith ""
        // System.Enum.GetNames(typeof<JTokenType>)
        // |> Array.map (sprintf "JTokenType.%s")
        // |> String.concat "\n"
        // |> Clipboard.setText
        let rec toJToken = function
            | Scalar x ->
                match x with
                | BoolTyp x -> JToken.FromObject x
                | IntegerTyp x -> JToken.FromObject x
                | StringTyp x -> JToken.FromObject x
                | FloatTyp x -> JToken.FromObject x
                | DateTimeTyp x -> JToken.FromObject x
            | Null -> JValue.CreateNull() :> JToken
            | Obj m ->
                let o = JObject()
                m |> Map.iter (fun k x -> o.Add(k, toJToken x) )
                o :> JToken
            | Sequence xs ->
                let ja = JArray()
                Array.iter (fun x -> toJToken x |> ja.Add) xs
                ja :> JToken
        // let test () =
        //     let path = @"CommonEvents.json"
        //     let json = Json.JToken.ofFile path
        //     let fs = ofJToken json
        //     let act = toJToken fs |> ofJToken
        //     fs = act
    let objToFSharpType x =
        x
        |> JsonF.getObj
        |> flip (Map.foldBack (fun k v st ->
            let x =
                match v with
                | JsonF.Scalar x ->
                    match x with
                    | JsonFType.BoolTyp _ -> "bool"
                    | JsonFType.FloatTyp _ -> "double"
                    | JsonFType.IntegerTyp _ -> "int"
                    | JsonFType.StringTyp _ -> "string"
                    | JsonFType.DateTimeTyp(_) -> "System.DateTime"
                | JsonF.Null -> "null"
                | JsonF.Sequence _ -> "FSharpJsonType.JsonF []"
                | JsonF.Obj _ -> "FSharpJsonType.JsonF"
                // | _ -> "FSharpJsonType.JsonF"
            (k, x)::st
            )) []
        |> List.map (curry (sprintf "    %s : %s"))
        |> String.concat "\n"
        |> sprintf "{\n%s\n}"

    [<RequireQualifiedAccess>]
    module Serialize =
        type Converter() =
            inherit Newtonsoft.Json.JsonConverter<JsonF>()
            override __.ReadJson(reader: Newtonsoft.Json.JsonReader,
                                 objectType: System.Type,
                                 existingValue:JsonF,
                                 hasExistingValue,
                                 serializer: Newtonsoft.Json.JsonSerializer) =
                serializer.Deserialize<Newtonsoft.Json.Linq.JToken>(reader)
                |> JsonF.ofJToken

            override __.WriteJson(writer: Newtonsoft.Json.JsonWriter,
                                  value:JsonF,
                                  serializer: Newtonsoft.Json.JsonSerializer) =
                // let xs = value :?> FsharpMyExtension.Json.FSharpJsonType.JsonF
                JsonF.toJToken value
                |> fun x -> serializer.Serialize(writer, x)
        let converter = Converter()
        let ser x =
            let indent = Newtonsoft.Json.Formatting.Indented
            Newtonsoft.Json.JsonConvert.SerializeObject(x, indent, converter)
        let serf path x =
            // ser x |> fun x -> System.IO.File.WriteAllText(path, x)
            use file = System.IO.File.CreateText(path)
            use st = new Newtonsoft.Json.JsonTextWriter(file)
            st.Formatting <- Newtonsoft.Json.Formatting.Indented
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Serialize(st, x)
        let serNotIndent x =
            Newtonsoft.Json.JsonConvert.SerializeObject(x, converter)
        let serfNotIdent path x =
            // serNotIndent x
            // |> fun x -> System.IO.File.WriteAllText(path, x)
            use file = System.IO.File.CreateText(path)
            use st = new Newtonsoft.Json.JsonTextWriter(file)
            // st.Formatting <- Newtonsoft.Json.Formatting.None
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Serialize(st, x)
        let des x = Newtonsoft.Json.JsonConvert.DeserializeObject<_>(x, converter)
        let desf path =
            use st = System.IO.File.OpenText path
            use r = new Newtonsoft.Json.JsonTextReader(st)
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Deserialize<_>(r)
            // Newtonsoft.Json.Linq.JToken.ReadFrom(r)
            // System.IO.File.ReadAllText path |> des
    [<RequireQualifiedAccess>]
    module SerializeOption =
        open System
        open Microsoft.FSharp.Reflection
        open Newtonsoft.Json
        /// Взято [отсюда](https://stackoverflow.com/a/29629215)
        type OptionConverter() =
            inherit JsonConverter()
            override x.CanConvert(t) =
                t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

            override x.WriteJson(writer, value, serializer) =
                let value =
                    if isNull value then null
                    else
                        let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                        fields.[0]
                serializer.Serialize(writer, value)

            override x.ReadJson(reader, t, existingValue, serializer) =
                let innerType = t.GetGenericArguments().[0]
                let innerType =
                    if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                    else innerType
                let value = serializer.Deserialize(reader, innerType)
                let cases = FSharpType.GetUnionCases(t)
                if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
                else FSharpValue.MakeUnion(cases.[1], [|value|])
        let converter = OptionConverter()
        let ser x =
            let indent = Newtonsoft.Json.Formatting.Indented
            Newtonsoft.Json.JsonConvert.SerializeObject(x, indent, converter)
        let serf path x =
            // ser x |> fun x -> System.IO.File.WriteAllText(path, x)
            use file = System.IO.File.CreateText(path)
            use st = new Newtonsoft.Json.JsonTextWriter(file)
            st.Formatting <- Newtonsoft.Json.Formatting.Indented
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Serialize(st, x)
        let serNotIndent x =
            Newtonsoft.Json.JsonConvert.SerializeObject(x, converter)
        let serfNotIdent path x =
            // serNotIndent x
            // |> fun x -> System.IO.File.WriteAllText(path, x)
            use file = System.IO.File.CreateText(path)
            use st = new Newtonsoft.Json.JsonTextWriter(file)
            // st.Formatting <- Newtonsoft.Json.Formatting.None
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Serialize(st, x)
        let des x = Newtonsoft.Json.JsonConvert.DeserializeObject<_>(x, converter)
        let desf path =
            use st = System.IO.File.OpenText path
            use r = new Newtonsoft.Json.JsonTextReader(st)
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add converter
            ser.Deserialize<_>(r)
            // Newtonsoft.Json.Linq.JToken.ReadFrom(r)
            // System.IO.File.ReadAllText path |> des
