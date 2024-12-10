module Tests.Day9

open Xunit
open Day9.Part1
open Day9.Part2

[<Fact>]
let ``part1: 12345`` () =
    Assert.Equal(60L, part1("12345"))

[<Fact>]
let ``part1: 2333133121414131402`` () =
    Assert.Equal(1928L, part1("2333133121414131402"))

[<Fact>]
let ``part2: 12345`` () =
    Assert.Equal(132L, part2("12345"))

[<Fact>]
let ``part2: 2333133121414131402`` () =
    Assert.Equal(2858L, part2("2333133121414131402"))
