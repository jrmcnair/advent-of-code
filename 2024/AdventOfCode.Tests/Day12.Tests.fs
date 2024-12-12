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

module neighbors =
    let plots = getPlots "123\r\n456\r\n789"

    [<Fact>]
    let ``Plots.neighbors: upper left`` () =
        let expected = [ ((0,1), '2'); ((1,0), '4') ]
        let actual = Plots.neighbors plots (0,0)
        Assert.Equivalent(expected, actual)

    let ``Plots.neighbors: upper mid`` () =
        let expected = [ ((0,1), '1'); ((0,2), '3'); ((1,1), '5') ]
        let actual = Plots.neighbors plots (0,1)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: upper right`` () =
        let expected = [ ((0,1), '2'); ((1,2), '6') ]
        let actual = Plots.neighbors plots (0,2)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: mid left`` () =
        let expected = [ ((0,0), '1'); ((1,1), '5'); ((2,0), '7') ]
        let actual = Plots.neighbors plots (1,0)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors:center`` () =
        let expected = [ ((1,0), '4'); ((0,1), '2'); ((1,2), '6'); ((2,1), '8') ]
        let actual = Plots.neighbors plots (1,1)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: mid right`` () =
        let expected = [ ((0,2), '3'); ((1,1), '5'); ((2,2), '9') ]
        let actual = Plots.neighbors plots (1,2)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: lower left`` () =
        let expected = [ ((1,0), '4'); ((2,1), '8') ]
        let actual = Plots.neighbors plots (2,0)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: lower mid`` () =
        let expected = [ ((2,0), '7'); ((1,1), '5'); ((2,2), '9') ]
        let actual = Plots.neighbors plots (2,1)
        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``Plots.neighbors: lower right`` () =
        let expected = [ ((2,1), '8'); ((1,2), '6') ]
        let actual = Plots.neighbors plots (2,2)
        Assert.Equivalent(expected, actual)

module Part1 =
    open Day12.Part1

    module regionFinder =

        [<Fact>]
        let ``single plot`` () =
            let visited : HashSet<Coord> = HashSet()
            let plots = getPlots "X"
            let expected = { Area = 1; Fences = 4 }
            Assert.Equal(expected, regionFinder plots visited (0,0))

        [<Fact>]
        let ``all one plant`` () =
            let visited : HashSet<Coord> = HashSet()
            let plots = getPlots "XX\r\nXX"
            let expected = { Area = 4; Fences = 8 }
            Assert.Equal(expected, regionFinder plots visited (0,0))

        [<Fact>]
        let ``two plants`` () =
            let visited : HashSet<Coord> = HashSet()
            let plots = getPlots "YXX\r\nYYX\r\nYYY"
            let expected = { Area = 3; Fences = 8 }
            Assert.Equal(expected, regionFinder plots visited (0,1))

    module execute =
        let plots = getPlots sample

        [<Fact>]
        let ``run sample`` () =
            let expected = 1930
            Assert.Equal(expected, execute plots)

module Part2 =
    open Day12.Part2 

    module execute =
        let plots = getPlots sample

        [<Fact>]
        let ``run sample`` () =
            let expected = 1206
            Assert.Equal(expected, execute plots)
