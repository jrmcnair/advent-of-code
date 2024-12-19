module Tests.Day19

open System
open Xunit
open Day19

let parseInput (raw: string) =
    raw.Split(Environment.NewLine)
    |> parse

let sample = "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

module Parse =

    [<Fact>]
    let ``parse produces correct collections`` () =
        let input = "w, u, b, gr, gb

wubrg
grgrgr"
        let expectedPatterns = [ "w"; "u"; "b"; "gr"; "gb" ]
        let expectedDesigns = seq { "wubrg"; "grgrgr" }

        let patterns, designs = input.Split(Environment.NewLine) |> parse 
        Assert.Equivalent(expectedPatterns, patterns)
        Assert.Equivalent(expectedDesigns, designs)

    [<Fact>]
    let ``parse sample produces correct collections`` () =
        let expectedPatterns = [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ]
        let expectedDesigns = seq { "brwrr"; "bggr"; "gbbr"; "rrbgbr"; "ubwu"; "bwurrg"; "brgr"; "bbrgwb" }
        let patterns, designs = sample.Split(Environment.NewLine) |> parse 

        Assert.Equivalent(expectedPatterns, patterns)
        Assert.Equivalent(expectedDesigns, designs)

module Part1 =
    
    [<Theory>]
    [<InlineData("brwrr", 1)>]
    [<InlineData("bggr", 1)>]
    [<InlineData("gbbr", 1)>]
    [<InlineData("rrbgbr", 1)>]
    [<InlineData("ubwu", 0)>]
    [<InlineData("bwurrg", 1)>]
    [<InlineData("brgr", 1)>]
    [<InlineData("bbrgwb", 0)>]
    let ``part1 for each design in sample individually`` (design: string, expected: int) =
        let patterns = [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ]
        Assert.Equal(expected, part1 patterns [ design ])

    [<Fact>]
    let ``part1 with sample input`` () =
        let patterns, designs = sample.Split(Environment.NewLine) |> parse
        let expected = 6
        
        Assert.Equal(expected, part1 patterns designs)

module Part2 =
    
    [<Theory>]
    [<InlineData("brwrr", 2L)>]
    [<InlineData("bggr", 1L)>]
    [<InlineData("gbbr", 4L)>]
    [<InlineData("rrbgbr", 6L)>]
    [<InlineData("ubwu", 0L)>]
    [<InlineData("bwurrg", 1L)>]
    [<InlineData("brgr", 2L)>]
    [<InlineData("bbrgwb", 0L)>]
    let ``part2 for each design individually`` (design: string, expected: int64) =
        let patterns = [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ]
        Assert.Equal(expected, part2 patterns [ design ])

    [<Fact>]
    let ``part2 with full sample`` () =
        let patterns, designs = sample.Split(Environment.NewLine) |> parse
        let expected = 16L
        
        Assert.Equal(expected, part2 patterns designs)
