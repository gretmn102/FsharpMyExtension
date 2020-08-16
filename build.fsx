// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.IO.Globbing.Operators
open Fake.Core
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )
let testProjName = "Test"
let testProjPath = f testProjName
let mainProjName = "FsharpMyExtension"
let mainProjPath = f mainProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release
let dotnetSdk = lazy DotNet.install DotNet.Versions.FromGlobalJson
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "BuildTest" (fun _ ->
    testProjPath
    |> Fake.IO.Path.getDirectory
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
        |> dtntSmpl)
)

Target.create "NuGet" (fun _ ->
    mainProjPath
    |> System.IO.Path.GetDirectoryName
    |> DotNet.pack (fun x ->
        { x with Configuration = DotNet.BuildConfiguration.Release }
        |> dtntSmpl
    )
)

Target.create "PushNuGetToGithub" (fun _ ->
    let packPath = !! "**/*.nupkg" |> Seq.tryHead
    packPath
    |> Option.defaultWith (fun () -> failwith "'**/*.nupkg' not found")
    |> DotNet.nugetPush (fun x ->
        { x with
            PushParams = { x.PushParams with Source = Some "github" }}
    )
)

let run projName projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/net461/%s.exe" buildConf projName
    let path = Fake.IO.Path.combine dir localpath
    if not <| Fake.IO.File.exists path then
        failwithf "not found %s" path

    Command.RawCommand(path, Arguments.Empty)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory (Fake.IO.Path.getDirectory path)
    |> Proc.run

Target.create "Test" (fun _ ->
    let x = run testProjName testProjPath
    if x.ExitCode <> 0 then
        failwith "test error"
)

// Target "Release" DoNothing
// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"NuGet"
  ==> "PushNuGetToGithub"

"BuildTest"
  ==> "Test"
Target.runOrDefault "Test"