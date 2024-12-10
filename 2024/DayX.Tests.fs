module Tests.Day?

open System
open Xunit
open Day?

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

[<Fact>]
let ``scratchpad`` () =
    let input = loadInput ""
    ()

[<Fact>]
let ``part1: sample`` () =
    let input = loadInput ""
    let expected = 0

    Assert.Fail "Not Implemented"
    Assert.Equal(expected, part1 input)


[<Fact>]
let ``part2: sample`` () =
    let input = loadInput ""
    let expected = 0

    Assert.Fail "Not Implemented"
    Assert.Equal(expected, part2 input)
