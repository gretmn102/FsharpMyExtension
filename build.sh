#!/bin/bash

dotnet tool restore
dotnet paket install
dotnet fsi build.fsx $@
