module Tests.Day16

open System
open Xunit
open Day16

let sample1 = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

let sample2 = "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

module Parse =
    
    [<Fact>]
    let ``all chars`` () =
        let input = loadInput "SE#."
        let expected = array2D [ [Start; End; Wall; Empty ] ]

        Assert.Equal(expected, parse input)

module Part1 =

    [<Fact>]
    let ``single move`` () =
        let input = loadInput "####
#SE#
####"
        let expected = 1
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``turn right + single move`` () =
        let input = loadInput "###
#S#
#E#
###"
        let expected = 1001
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``turn left + single move`` () =
        let input = loadInput "###
#E#
#S#
###"
        let expected = 1001
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``two choices`` () =
        let input = loadInput "######
#...##
#.#.##
#...E#
###S##
######"
        let expected = 2002
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``sample1`` () =
        let input = loadInput sample1
        let expected = 7036
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``sample2`` () =
        let input = loadInput sample2
        let expected = 11048
        let actual, _ = parse input |> findBestScore

        Assert.Equal(expected, actual)

module Part2 =

    [<Fact>]
    let ``single move`` () =
        let input = loadInput "####
#SE#
####"
        let expected = 2
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``turn right + single move`` () =
        let input = loadInput "###
#S#
#E#
###"
        let expected = 2
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``turn left + single move`` () =
        let input = loadInput "###
#E#
#S#
###"
        let expected = 2
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``two choices`` () =
        let input = loadInput "######
#...##
#.#.##
#...E#
###S##
######"
        let expected = 3
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``sample1`` () =
        let input = loadInput sample1
        let expected = 45
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``sample2`` () =
        let input = loadInput sample2
        let expected = 64
        let _, actual = parse input |> findBestScore

        Assert.Equal(expected, actual)
