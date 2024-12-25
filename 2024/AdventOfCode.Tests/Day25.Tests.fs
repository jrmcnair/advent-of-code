module Tests.Day25

open System.Collections.Generic
open Xunit
open Day25
open AdventOfCode.Tests.Common

let sample = "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

[<Fact>]
let ``parse sample`` () =
    let input = sample |> loadInput
    let expected = dict [
        Lock, [ [| 0;5;3;4;3 |]; [| 1;2;0;5;3 |] ]
        Key, [ [| 5;0;2;1;3 |]; [| 4;3;4;0;2 |]; [| 3;0;2;0;1 |] ]
    ]

    Assert.Equivalent(expected, parse input)

[<Fact>]
let ``keyFitsLock is false when there are overlaps`` () =
    let key = [| 5;0;2;1;3 |]
    let lock = [| 0;5;3;4;3 |]

    Assert.False(keyFitsLock key lock)

[<Fact>]
let ``keyFitsLock is false when there are no overlaps`` () =
    let key = [| 3;0;2;0;1 |]
    let lock = [| 0;5;3;4;3 |]

    Assert.True(keyFitsLock key lock)

[<Fact>]
let ``countKeysThatFit sample`` () =
    let input = sample |> loadInput |> parse
    let expected = 3

    Assert.Equivalent(expected, countKeysThatFit input)
