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

module IsPossible =
    
    [<Theory>]
    [<InlineData("brwrr", true)>]
    [<InlineData("bggr", true)>]
    [<InlineData("gbbr", true)>]
    [<InlineData("rrbgbr", true)>]
    [<InlineData("ubwu", false)>]
    [<InlineData("bwurrg", true)>]
    [<InlineData("brgr", true)>]
    [<InlineData("bbrgwb", false)>]
    let ``isPossible returns correct values`` (design: string, expected: bool) =
        let patterns = [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ]
        Assert.Equal(expected, isPossible patterns design)

