// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "nuget: Fake.Core.Target, 6.1.4"
#r "nuget: Fake.Core.ReleaseNotes, 6.1.4"
#r "nuget: Fake.IO.FileSystem, 6.1.4"
#r "nuget: Fake.DotNet.Cli, 6.1.4"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
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

let release = ReleaseNotes.load "RELEASE_NOTES.md"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml

let initViaFsi () =
    System.Environment.GetCommandLineArgs()
    |> Array.skip 2 // skip fsi.exe; build.fsx
    |> Array.toList
    |> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
    |> Fake.Core.Context.RuntimeContext.Fake
    |> Fake.Core.Context.setExecutionContext

    Target.initEnvironment ()
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
initViaFsi ()

Target.create "Clean" (fun _ ->
    let cleanBinAndObj projectPath =
        Shell.cleanDirs [
            projectPath </> "bin"
            projectPath </> "obj"
        ]
    cleanBinAndObj mainProjDir
    cleanBinAndObj testsProjDir
    Shell.cleanDir deployDir
)

Target.create "Meta" (fun _ ->
    [
        "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
        "<ItemGroup>"
        "    <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
        "</ItemGroup>"
        "<PropertyGroup>"
        "    <EmbedUntrackedSources>true</EmbedUntrackedSources>"
        "    <PackageProjectUrl>https://github.com/gretmn102/FsharpMyExtension</PackageProjectUrl>"
        "    <PackageLicenseExpression>MIT</PackageLicenseExpression>"
        "    <RepositoryUrl>https://github.com/gretmn102/FsharpMyExtension.git</RepositoryUrl>"
        sprintf "    <PackageReleaseNotes>%s</PackageReleaseNotes>"
            (String.concat "\n" release.Notes |> XmlText.escape)
        "    <PackageTags>interactive-fiction;fsharp</PackageTags>"
        "    <Authors>Fering</Authors>"
        sprintf "    <Version>%s</Version>" (string release.SemVer)
        "</PropertyGroup>"
        "</Project>"
    ]
    |> File.write false "Directory.Build.props"
)

let commonBuildArgs = "-c Release"

Target.create "Build" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "build %s" commonBuildArgs)
)

Target.create "Deploy" (fun _ ->
    let target = "-f net461"
    mainProjDir
    |> dotnet (sprintf "build %s %s -o \"%s\"" commonBuildArgs target deployDir)
)

Target.create "Pack" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "pack %s -o \"%s\"" commonBuildArgs deployDir)
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
    |> dotnet (sprintf "build %s" commonBuildArgs)
)

Target.create "RunTestsNet461" (fun _ ->
    testsProjDir
    |> dotnet (sprintf "run %s -f net461" commonBuildArgs)
)

Target.create "RunTestsNetcoreapp3.1" (fun _ ->
    testsProjDir
    |> dotnet (sprintf "run %s -f netcoreapp3.1" commonBuildArgs)
)

Target.create "FableBuild" (fun _ ->
    mainProjDir
    |> dotnet "fable -o bin/fable"
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Build"

"Clean"
  ==> "Deploy"

"Clean"
  ==> "Meta"
  ==> "Pack"
  ==> "PushToGitlab"

"BuildTests"

"RunTestsNet461"

try
    Target.runOrDefault "Deploy"
    0
with e ->
    eprintfn "%A" e.Message
    1
|> exit
