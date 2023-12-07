module FsharpMyExtension.Enum.Tests
open Fuchu
open FsharpMyExtension

type UserRole =
    | Admin = 0b01
    | Member = 0b10
    | AdminAndMember = 0b11

[<Tests>]
let ``Enum.is`` =
    let createTest (current, value, exp) =
        testCase (sprintf "%A is %A = %b" current value exp) <| fun () ->
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
    let createTest (current, value, exp) =
        testCase (sprintf "%A contains %A = %b" current value exp) <| fun () ->
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
