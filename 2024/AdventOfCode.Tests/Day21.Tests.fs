module Tests.Day21

open Xunit
open Day21

let codes = ["029A"; "980A"; "179A"; "456A"; "379A" ]

module Commands =
    
    [<Fact>]
    let ``robot 1 commands for 029A`` () =
        let buttons = "029A"
        let expected = "<A^A^^>AvvvA"
        
        Assert.Equal(expected, getKeypadCommands buttons)
    
    [<Fact>]
    let ``robot 2 commands for 029A`` () =
        let buttons = "<A^A^^>AvvvA"
        let expected = "v<<A>>^A<A>A<AAv>A^A<vAAA^>A"
        
        Assert.Equal(expected, getRemoteCommands buttons)
    
    [<Fact>]
    let ``robot 3 commands for 029A`` () =
        let buttons = "v<<A>>^A<A>A<AAv>A^A<vAAA^>A"
        let expectedCommands = "<vA<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AA<vA>A^A<A>Av<<A>A^>AAA<Av>A^A"
        let expectedComplexity = 1972
        let actual = getRemoteCommands buttons

        Assert.Equal(expectedCommands, actual)
        Assert.Equal(expectedComplexity, complexity "029A" actual)
    
    [<Fact>]
    let ``robot 1 commands for 379A`` () =
        let buttons = "379A"
        let expected = "^A<<^^A>>AvvvA"
        
        Assert.Equal(expected, getKeypadCommands buttons)
    
    [<Fact>]
    let ``robot 2 commands for 379A`` () =
        let buttons = "^A<<^^A>>AvvvA"
        let expected = "<A>Av<<AA>^AA>AvAA^A<vAAA^>A"
        
        Assert.Equal(expected, getRemoteCommands buttons)
    
    [<Fact>]
    let ``robot 3 commands for 379A`` () =
        let buttons = "<A>Av<<AA>^AA>AvAA^A<vAAA^>A"
        let expectedCommands = "v<<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA^>AA<A>Av<<A>A^>AAA<Av>A^A"
        let expectedComplexity = 24256

        let actual = getRemoteCommands buttons
        
        Assert.Equal(expectedCommands, actual)
        Assert.Equal(expectedComplexity, complexity "379A" actual)

module Complexity =

    [<Theory>]
    [<InlineData("029A", "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A", 1972)>]
    [<InlineData("980A", "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A", 58800)>]
    [<InlineData("179A", "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 12172)>]
    [<InlineData("456A", "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A", 29184)>]
    [<InlineData("379A", "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A", 24256)>]
    let ``complexity is correct`` (code:string, cmds: string, expected: int) =
        Assert.Equal(expected, complexity code cmds)

module Part1 =

    [<Theory>]
    [<InlineData("029A", 1972)>]
    [<InlineData("980A", 58800)>]
    [<InlineData("179A", 12172)>]
    [<InlineData("456A", 29184)>]
    [<InlineData("379A", 24256)>]
    let ``part1 on single sample codes`` (code, expected) =
        Assert.Equal(expected, solve 2 [ code ])

