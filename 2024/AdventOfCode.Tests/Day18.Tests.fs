module Tests.Day18

open System
open Xunit
open Day18

let sampleSize = 6
let sampleTicks = 12
let sample = "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

[<Fact>]
let ``parse multiple coords`` () =
    let input = loadInput "5,4\r\n4,2\r\n4,5"
    let expected = [ (5,4); (4,2); (4,5) ]

    Assert.Equivalent(expected, parse input)
    
[<Fact>]
let ``compute memoryState`` () =
    let coords = loadInput "0,0\r\n0,1\r\n2,1\r\n2,2" |> parse
    let size = 3
    let ticks = 3
    let expected = array2D [
        [ false; false; true; ]
        [ true; true; true; ]
        [ true; false; true; ]
    ]

    Assert.Equivalent(expected, memoryState coords size ticks)

[<Fact>]
let ``part1: sample`` () =
    let coords = loadInput sample |> parse
    let grid = memoryState coords sampleSize sampleTicks
    let expected = 22
    
    Assert.Equal(expected, findShortestPath grid sampleSize)