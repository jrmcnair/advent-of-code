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
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let data = sampleData.Split('\n')
        let expected = 4

        let actual = Seq.length data

        Assert.Equal(expected, actual)
