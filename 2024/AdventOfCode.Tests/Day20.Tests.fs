module Tests.Day20

open System
open Xunit
open Day20

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

let sample = "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

[<Fact>]
let ``parse initial test`` () =
    let input = loadInput "#.SE"
    
    let expected = array2D [[ '#'; '.'; 'S'; 'E' ]]

    Assert.Equal(expected, parse input)

[<Fact>]
let ``parse sample`` () =
    let input = loadInput sample
    let result = parse input

    Assert.Equal(15, Array2D.length1 result)
    Assert.Equal(15, Array2D.length2 result)
    Assert.Equal('S', result[3,1])
    Assert.Equal('E', result[7,5])

[<Fact>]
let ``race sample with no changes`` () =
    let track = loadInput sample |> parse
    let expected = Some 84

    Assert.Equal(expected, race track)

[<Fact>]
let ``interiorWalls: all can  cross`` () =
    let input = "######
#....#
#.##.#
#....#
######"
    let track = loadInput input |> parse
    let expected = [ (2,2) ]
    let actual = interiorWalls track

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``interiorWalls: some cannot cross`` () =
    let input = "#######
##...##
#..#..#
#.###.#
#..#..#
##...##
#######"
    let track = loadInput input |> parse
    let expected = [ (2,3); (3,2); (3,4); (4,3) ]
    let actual = interiorWalls track

    Assert.Equivalent(expected, actual)
    
[<Theory>]
[<InlineData(2, 44)>]
[<InlineData(4, 30)>]
[<InlineData(6, 16)>]
[<InlineData(8, 14)>]
[<InlineData(10, 10)>]
[<InlineData(12, 8)>]
[<InlineData(20, 5)>]
[<InlineData(36, 4)>]
[<InlineData(38, 3)>]
[<InlineData(40, 2)>]
[<InlineData(64, 1)>]
let ``part1: sample w/ different picoseconds`` (timeSaved: int, expected: int) =
    let input = sample |> loadInput |> parse

    Assert.Equal(expected, part1 input timeSaved)

[<Fact>]
let ``part2 test`` () =
    let expected = ()
    Assert.Equal(expected, part2 ())
