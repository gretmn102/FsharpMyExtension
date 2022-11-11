// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
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
let testsProjDir = Path.getDirectory testProjPath
let mainProjName = "FsharpMyExtension"
let mainProjPath = f mainProjName
let mainProjDir = Path.getDirectory mainProjPath

let deployDir = Path.getFullName "./deploy"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "Build" (fun _ ->
    mainProjDir
    |> dotnet "build -c Release"
)

Target.create "Deploy" (fun _ ->
    let target = "-f net461"
    mainProjDir
    |> dotnet (sprintf "build -c Release -o \"%s\" %s" deployDir target)
)

Target.create "Pack" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "pack -c Release -o \"%s\"" deployDir)
)

Target.create "PushToGitlab" (fun _ ->
    let packPathPattern = sprintf "%s/*.nupkg" deployDir
    let packPath =
        !! packPathPattern |> Seq.tryExactlyOne
        |> Option.defaultWith (fun () -> failwithf "'%s' not found" packPathPattern)

    deployDir
    |> dotnet (sprintf "nuget push -s %s %s" "gitlab" packPath)
)

Target.create "BuildTests" (fun _ ->
    testsProjDir
    |> dotnet "build -c Release"
)

Target.create "RunTestsNet461" (fun _ ->
    testsProjDir
    |> dotnet "run -c Release -f net461"
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Build"

"Clean"
  ==> "Deploy"

"Clean"
  ==> "Pack"
  ==> "PushToGitlab"

"BuildTests"

"RunTestsNet461"

Target.runOrDefault "Deploy"
