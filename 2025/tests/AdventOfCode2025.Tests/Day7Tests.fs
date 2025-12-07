module Tests.Day7

open Xunit
open AdventOfCode2025.Day7

let sampleData = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............""".Split(System.Environment.NewLine)

module Part1 =

    [<Fact>]
    let ``Get Start returns correct coordinate`` () =
        let actual = sampleData |> Parse.start
        Assert.Equal(7, actual)
    
    [<Fact>]
    let ``Get Splitters returns correct coords`` () =
        let splitters = sampleData |> Parse.splitters
        Assert.Equal(22, splitters.Length)
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let start = sampleData |> Parse.start
        let splitters = sampleData |> Parse.splitters
        let maxRow = sampleData.Length - 1
        let maxCol = sampleData[0].Length - 1
        let expected = 21
        
        let actual = Part1.countSplits maxRow maxCol start splitters
        
        Assert.Equal(expected, actual)

module Part2 =

    [<Fact>]
    let ``Run Sample Data`` () =
        Assert.True(true)
