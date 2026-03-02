cls

dotnet tool restore
dotnet paket install
dotnet fsi build.fsx %*
