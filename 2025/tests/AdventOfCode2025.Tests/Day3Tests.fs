module Tests.Day3

open System
open AdventOfCode2025.Day3
open Xunit

let sampleData = """987654321111111
811111111111119
234234234234278
818181911112111"""

module Part1 =
    open Part1

    [<Theory>]
    [<InlineData("987654321111111", 98)>]
    [<InlineData("811111111111119", 89)>]
    [<InlineData("234234234234278", 78)>]
    [<InlineData("818181911112111", 92)>]
    let ``getMaxJoltage Tests`` (input: string, expected: int) =
        let actual = getMaxJoltage input
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Run Sample Data`` () =
        let expected = 357
        let actual = 
            sampleData.Split(Environment.NewLine)
            |> Seq.map getMaxJoltage
            |> Seq.sum

        Assert.Equal(expected, actual)

module Part2 =
    open Part2

    [<Theory>]
    [<InlineData("123", 2, 23L)>]
    let ``getMaxJoltage Tests for simple data items`` (input: string, numBatteries: int, expected: int64) =
        let actual = getMaxJoltage numBatteries input
        Assert.Equal(expected, actual)

    [<Theory>]
    [<InlineData("987654321111111", 987654321111L)>]
    [<InlineData("811111111111119", 811111111119L)>]
    [<InlineData("234234234234278", 434234234278L)>]
    [<InlineData("818181911112111", 888911112111L)>]
    let ``getMaxJoltage Tests for sample data items`` (input: string, expected: int64) =
        let actual = getMaxJoltage 12 input
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Run Sample Data`` () =
        let expected = 3121910778619L
        let actual = 
            sampleData.Split(Environment.NewLine)
            |> Seq.map (getMaxJoltage 12)
            |> Seq.sum

        Assert.Equal(expected, actual)
