module FsharpMyExtension.Enum.Tests
open Fuchu
open FsharpMyExtension

type UserRole =
    | Admin = 0b01
    | Member = 0b10

[<Tests>]
let ``Enum.is`` =
    let createTest (value, current, exp) =
        testCase (sprintf "is %A %A = %b" value current exp) <| fun () ->
            Assert.Equal("", exp, is value current)

    testList "Enum.is" [
        yield!
            [
                UserRole.Admin, UserRole.Admin, true
                UserRole.Member, UserRole.Admin, false
                UserRole.Admin, UserRole.Member, false
                UserRole.Member, UserRole.Member, true
                (UserRole.Admin ||| UserRole.Member), UserRole.Admin, false
                (UserRole.Member ||| UserRole.Admin), UserRole.Admin, false
            ]
            |> List.map createTest
    ]

[<Tests>]
let ``Enum.contains`` =
    let createTest (value, current, exp) =
        testCase (sprintf "contains %A %A = %b" value current exp) <| fun () ->
            Assert.Equal("", exp, contains value current)

    testList "Enum.contains" [
        yield!
            [
                UserRole.Admin, UserRole.Admin, true
                UserRole.Member, UserRole.Admin, false
                UserRole.Admin, UserRole.Member, false
                UserRole.Member, UserRole.Member, true
                (UserRole.Admin ||| UserRole.Member), UserRole.Admin, true
                (UserRole.Member ||| UserRole.Admin), UserRole.Admin, true
            ]
            |> List.map createTest
    ]
