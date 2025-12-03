module Tests.Day3

open AdventOfCode2025.Day3
open Xunit

let sampleData = """"""

module Part1 =
    open Part1
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let parsedData = parseData sampleData
        let expected = 0

        let actual = 0

        Assert.Equal(expected, actual)

module Part2 =
    open Part2
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let parsedData = parseData sampleData
        let expected = 0

        let actual = 0

        Assert.Equal(expected, actual)
