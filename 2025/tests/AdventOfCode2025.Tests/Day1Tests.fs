module Tests.Day1

open AdventOfCode2025.Day1
open Xunit

let sampleData = """
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
    """

module Part1 =
    open AdventOfCode2025.Day1.Part1

    [<Fact>]
    let ``Sample Data Test`` () =
        let start = 50
        let rotations = sampleData.Trim().Split("\n") |> parseData
        
        let password = getPassword rotations
        Assert.Equal(3, password)

module Part2 =
    open AdventOfCode2025.Day1.Part2
    
    [<Fact>]
    let  ``pass zero going right`` () =
        let start = 90
        let rotation = { Direction = Right; Distance = 15 }
        let expected = 5
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``land on zero going right`` () =
        let start = 50
        let rotation = { Direction = Right; Distance = 50 }
        let expected = 0
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``land on zero going right by one`` () =
        let start = 99
        let rotation = { Direction = Right; Distance = 1 }
        let expected = 0
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``land on zero going right by a complete rotation plus one`` () =
        let start = 99
        let rotation = { Direction = Right; Distance = 101 }
        let expected = 0
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``pass zero going left`` () =
        let start = 5
        let rotation = { Direction = Left; Distance = 10 }
        let expected = 95
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``land on zero going left`` () =
        let start = 50
        let rotation = { Direction = Left; Distance = 50 }
        let expected = 0
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)

    [<Fact>]
    let  ``land on zero going left by one`` () =
        let start = 1
        let rotation = { Direction = Left; Distance = 1 }
        let expected = 0
        
        let actual = executeRotation start rotation

        Assert.Equal(expected, actual)
