// [<RequireQualifiedAccess>]
module FsharpMyExtension.String
let split (sep:string) (s:string) = s.Split([|sep|], System.StringSplitOptions.None)
/// split [|"\r\n"; "\n"; "\r"|] s
let lines (s:string) = s.Split([|"\r\n"; "\n"; "\r"|], System.StringSplitOptions.None)
let replace (oldVal:string) newVal (s:string) = s.Replace(oldVal, newVal)
/// remove all white-spaces leading and trailing
let trim (s:string) = s.Trim()
let trimChar trimChar (s:string) = s.Trim [| trimChar |]
let trimChars trimChars (s:string) = s.Trim trimChars
let toLower (s:string) = s.ToLower()
let contains value (s:string) = s.Contains value
