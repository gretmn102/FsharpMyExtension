#!/bin/bash

dotnet tool restore
dotnet paket install
PAKET_SKIP_RESTORE_TARGETS=true
dotnet fake build $@
PAKET_SKIP_RESTORE_TARGETS=
