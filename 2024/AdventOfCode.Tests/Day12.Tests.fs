module Tests.Day12

open System
open System.Collections.Generic
open Xunit
open Day12

let sample = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let getPlots (input: string) =
    input
    |> _.Split(Environment.NewLine)
    |> Plots.load

module processNeighbors =
    let plots = getPlots "123\r\n456\r\n789"

    [<Fact>]
    let ``no neighbors `` () =
        let plots = getPlots "X"
        let expected = (Seq.empty, 4, 4)
        let actual = Plots.processNeighbors plots (0,0)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``one neighbor, left plant`` () =
        let plots = getPlots "XX"
        let expected = ([ (0,1) ], 3, 3)
        let actual = Plots.processNeighbors plots (0,0)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``one neighbor, right plant`` () =
        let plots = getPlots "XX"
        let expected = ([ (0,0) ], 3, 1)
        let actual = Plots.processNeighbors plots (0,1)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``box, bottom left`` () =
        let plots = getPlots "XX\r\nXX"
        let expected = ([ (0,0); (1,1) ], 2, 1)
        let actual = Plots.processNeighbors plots (1,0)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``odd shape, bottom right`` () =
        let plots = getPlots "XX\r\nYX"
        let expected = ([ (0,1) ], 3, 2)
        let actual = Plots.processNeighbors plots (1,1)
        Assert.Equivalent(expected, actual)

module regionFinder =

    [<Fact>]
    let ``single plot`` () =
        let visited : HashSet<Coord> = HashSet()
        let plots = getPlots "X"
        let expected = { Area = 1; Perimeter = 4; Sides = 4 }
        Assert.Equal(expected, regionFinder plots visited (0,0))

    [<Fact>]
    let ``all one plant`` () =
        let visited : HashSet<Coord> = HashSet()
        let plots = getPlots "XX\r\nXX"
        let expected = { Area = 4; Perimeter = 8; Sides = 4 }
        Assert.Equal(expected, regionFinder plots visited (0,0))

    [<Fact>]
    let ``two plants`` () =
        let visited : HashSet<Coord> = HashSet()
        let plots = getPlots "YXX\r\nYYX\r\nYYY"
        let expected = { Area = 3; Perimeter = 8; Sides = 6 }
        Assert.Equal(expected, regionFinder plots visited (0,1))

module execute =
    let plots = getPlots sample

    [<Fact>]
    let ``part1: sample`` () =
        let regions = plots |> toRegions
        let expected = 1930

        Assert.Equal(expected, part1 regions)

    [<Fact>]
    let ``part2: sample`` () =
        let regions = plots |> toRegions
        let expected = 1206

        Assert.Equal(expected, part2 regions)
