// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"
open System
open Fake.IO.Globbing.Operators
open Fake.Core
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let testPath = !! "**/test.fsproj" |> Seq.tryHead
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Debug
let dotnetSdk = lazy DotNet.install DotNet.Versions.Release_2_1_402
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "BuildTest" (fun _ ->
    testPath
    |> Option.defaultWith (fun () -> failwith "'**/test.fsproj' not found")
    |> System.IO.Path.GetDirectoryName
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
        |> dtntSmpl)
)
let mainProjPath = !! "**/FsharpMyExtension.fsproj" |> Seq.tryHead
Target.create "NuGet" (fun _ ->
    mainProjPath
    |> Option.defaultWith (fun () -> failwith "'**/FsharpMyExtension.fsproj' not found")
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

Target.create "Test" (fun _ ->
    testPath
    |> Option.bind (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        let d = sprintf @"bin\%A\net461\test.exe" buildConf
        let dir = System.IO.Path.Combine(dir, d)
        if System.IO.File.Exists dir then Some dir else None
    )
    |> Option.map (fun dir ->
        let result =
            Process.execSimple (fun info ->
                info.WithFileName dir) TimeSpan.MaxValue
        if result <> 0 then failwith "tests failed"
    )
    |> Option.defaultWith (fun () -> failwith "test not found" )
)

// Target "Release" DoNothing
// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators
// "Clean"
//   ==> "Restore"
//   ==> "Build"
//   ==> "Test"

"NuGet"
  ==> "PushNuGetToGithub"

"BuildTest"
  ==> "Test"
Target.runOrDefault "Test"