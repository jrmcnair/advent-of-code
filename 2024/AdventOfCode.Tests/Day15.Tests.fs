module Tests.Day15

open System
open Xunit
open AdventOfCode.Solutions.Common

let sample = "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

let sample2= "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

module Part1 =
    open Day15.Part1

    module Parse =
        
        [<Fact>]
        let ``all chars`` () =
            let input = loadInput "O.@#\r\n\r\n^v><"
            let expectedWarehouse = array2D [[ Box; Empty; Robot; Wall ]]
            let expectedDirections = [ N; S; E; W ]

            let aw, ad = parse input
            
            Assert.Equivalent(expectedWarehouse, aw)
            Assert.Equivalent(expectedDirections, ad)

    module FindEmpty =
        
        [<Fact>]
        let ``Next tile is empty`` () =
            let input = loadInput "@O.#\r\n\r\n>"
            let warehouse, _ = parse input
            let start = Loc.create 0 1
            let expected = Some ({ Row = 0; Col = 2 })

            Assert.Equal(expected, findEmpty warehouse E start)
        
        [<Fact>]
        let ``Empty tile is three tiles away empty`` () =
            let input = loadInput "@OOO.#\r\n\r\n>"
            let warehouse, _ = parse input
            let start = Loc.create 0 1
            let expected = Some ({ Row = 0; Col = 4 })

            Assert.Equal(expected, findEmpty warehouse E start)
        
        [<Fact>]
        let ``No empty tile, wall next tile`` () =
            let input = loadInput "@O#\r\n\r\n>"
            let warehouse, _ = parse input
            let start = Loc.create 0 1
            let expected = None

            Assert.Equal(expected, findEmpty warehouse E start)
        
        [<Fact>]
        let ``No empty tile, boxes before wall`` () =
            let input = loadInput "@OOO#\r\n\r\n>"
            let warehouse, _ = parse input
            let start = Loc.create 0 1
            let expected = None

            Assert.Equal(expected, findEmpty warehouse E start)

    module Move =
        
        [<Fact>]
        let ``Single move to empty tile does not change warehouse`` () =
            let input = loadInput "@.#\r\n\r\n>"
            let start = Loc.create 0 0
            let expected = array2D [[ Robot; Empty; Wall ]]

            let warehouse, dirs = parse input
            move warehouse dirs start

            Assert.Equivalent(expected, warehouse)            
        
        [<Fact>]
        let ``Single move to wall tile does not change warehouse`` () =
            let input = loadInput "@#\r\n\r\n>"
            let start = Loc.create 0 0
            let expected = array2D [[ Robot; Wall ]]

            let aw, ad = parse input
            move aw ad start

            Assert.Equivalent(expected, aw)            
        
        [<Fact>]
        let ``Single box with space after swaps`` () =
            let input = loadInput "@O.#\r\n\r\n>"
            let start = Loc.create 0 0
            let expected = array2D [[ Robot; Empty; Box; Wall ]]

            let aw, ad = parse input
            move aw ad start

            Assert.Equivalent(expected, aw)            
        
        [<Fact>]
        let ``Single box with wall behind does not change warehouse`` () =
            let input = loadInput "@O#.#\r\n\r\n>"
            let start = Loc.create 0 0
            let expected = array2D [[ Robot; Box; Wall; Empty; Wall ]]

            let aw, ad = parse input
            move aw ad start

            Assert.Equivalent(expected, aw)            
        
        [<Fact>]
        let ``Multiple boxes with space behind shifts`` () =
            let input = loadInput "@OO.#\r\n\r\n>"
            let start = Loc.create 0 0
            let expected = array2D [[ Robot; Empty; Box; Box; Wall ]]

            let aw, ad = parse input
            move aw ad start

            Assert.Equivalent(expected, aw)            

    module Solve =

        [<Fact>]
        let ``part1: small sample`` () =
            let input = loadInput sample
            let expected = 2028

            Assert.Equal(expected, execute input)

        [<Fact>]
        let ``part1: large sample`` () =
            let input = loadInput sample2
            let expected = 10092

            Assert.Equal(expected, execute input)
