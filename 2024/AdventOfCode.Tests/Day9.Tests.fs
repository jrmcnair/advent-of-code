module Tests.Day9

open Xunit
open Day9

[<Fact>]
let ``part1: 12345`` () =
    Assert.Equal(60L, Part1.run("12345"))

[<Fact>]
let ``part1: 2333133121414131402`` () =
    Assert.Equal(1928L, Part1.run("2333133121414131402"))

[<Fact>]
let ``part2: 12345`` () =
    Assert.Equal(132L, Part2.run("12345"))

[<Fact>]
let ``part2: 2333133121414131402`` () =
    Assert.Equal(2858L, Part2.run("2333133121414131402"))
