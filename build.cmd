cls

dotnet tool restore
dotnet paket install
SET PAKET_SKIP_RESTORE_TARGETS=true
dotnet fake build %*
SET PAKET_SKIP_RESTORE_TARGETS=
