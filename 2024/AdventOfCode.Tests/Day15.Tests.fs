module Tests.Day15

open System
open System.Collections.Generic
open Xunit
open AdventOfCode.Common

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
            let expected = Some { Row = 0; Col = 2 }

            Assert.Equal(expected, findEmpty warehouse E start)
        
        [<Fact>]
        let ``Empty tile is three tiles away empty`` () =
            let input = loadInput "@OOO.#\r\n\r\n>"
            let warehouse, _ = parse input
            let start = Loc.create 0 1
            let expected = Some { Row = 0; Col = 4 }

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

module Part2 =
    open Day15.Part2

    module Parse =
        
        [<Fact>]
        let ``all chars`` () =
            let input = loadInput "O.@#\r\n\r\n^v><"
            let expectedWarehouse = array2D [
                [ BoxLeft; BoxRight
                  Empty; Empty
                  Robot; Empty
                  Wall; Wall ]
            ]
            let expectedDirections = [ N; S; E; W ]

            let aw, ad = parse input
            
            Assert.Equivalent(expectedWarehouse, aw)
            Assert.Equivalent(expectedDirections, ad)

    module FindEmpty =
    
        [<Fact>]
        let ``Horizontal: next tile is empty`` () =
            let input = loadInput "#.O.#\r\n\r\n>>" // ##..[]..##
            let warehouse, _ = parse input
            let expected = HashSet<Loc>([ { Row = 0; Col = 4 }; { Row = 0; Col = 5 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse E { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse W { Row = 0; Col = 5 })
        
        [<Fact>]
        let ``Horizontal: empty tile is past multiple boxes tiles away empty`` () =
            let input = loadInput "#.OOO.#\r\n\r\n>" // ##..[][][]..##
            let warehouse, _ = parse input
            let expected = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 0; Col = 6 }; { Row = 0; Col = 7 }
                  { Row = 0; Col = 8 }; { Row = 0; Col = 9 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse E { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse W { Row = 0; Col = 9 })
        
        [<Fact>]
        let ``Horizontal: no empty tile, wall next tile`` () =
            let input = loadInput "#O#\r\n\r\n>" // ##[]##
            let warehouse, _ = parse input
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse E { Row = 0; Col = 2 })
            Assert.Equivalent(expected, findEmpty warehouse W { Row = 0; Col = 3 })
        
        [<Fact>]
        let ``Horizontal: no empty tile, wall directly after boxes`` () =
            let input = loadInput "#OO#\r\n\r\n>" // ##[][]##
            let warehouse, _ = parse input
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse E { Row = 0; Col = 2 })
            Assert.Equivalent(expected, findEmpty warehouse W { Row = 0; Col = 5 })
        
        [<Fact>]
        let ``Vertical: single box with space available`` () =
            // ##......##
            // ##..[]..##
            // ##......##
            let input = loadInput "#...#\r\n#.O.#\r\n#...#\r\n\r\n>"
            let warehouse, _ = parse input
            let expected = HashSet<Loc>(
                [ { Row = 1; Col = 4 }; { Row = 1; Col = 5 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 1; Col = 5 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 5 })
        
        [<Fact>]
        let ``Vertical: single box with wall`` () =
            // ##########
            // ##..[]..##
            // ##########
            let input = loadInput "#####\r\n#.O.#\r\n#####\r\n\r\n>"
            let warehouse, _ = parse input
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 1; Col = 5 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 5 })
        
        [<Fact>]
        let ``Vertical: stacked boxes with space available`` () =
            // ##......##
            // ##..[]..##
            // ##..[]..##
            // ##......##
            let input = loadInput "#...#\r\n#.O.#\r\n#.O.#\r\n#...#\r\n\r\n>"
            let warehouse, _ = parse input
            let expected = HashSet<Loc>(
                [ { Row = 1; Col = 4 }; { Row = 1; Col = 5 }
                  { Row = 2; Col = 4 }; { Row = 2; Col = 5 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 5 })
        
        [<Fact>]
        let ``Vertical: perfectly stacked boxes with wall`` () =
            // ##########
            // ##..[]..##
            // ##..[]..##
            // ##########
            let input = loadInput "#####\r\n#.O.#\r\n#.O.#\r\n#####\r\n\r\n>"
            let warehouse, _ = parse input
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 1; Col = 5 })
        
        [<Fact>]
        let ``Vertical: double stack with space available to N`` () =
            // ##......##
            // ##.[][].##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let expected = HashSet<Loc>(
                [ { Row = 2; Col = 4 }; { Row = 2; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 }
                  { Row = 1; Col = 5 }; {Row = 1; Col = 6 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
        
        [<Fact>]
        let ``Vertical: double stack with wall to N`` () =
            // ##########
            // ##.[][].##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
        
        [<Fact>]
        let ``Vertical: double stack with space available to S`` () =
            // ##..[]..##
            // ##.[][].##
            // ##......##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                ]
            let expected = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 }
                  { Row = 1; Col = 5 }; {Row = 1; Col = 6 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 5 })
        
        [<Fact>]
        let ``Vertical: double stack with wall to S`` () =
            // ##..[]..##
            // ##.[][].##
            // ##########
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall;  Wall; Wall;  Wall; Wall;  Wall; Wall; Wall; Wall ]
                ]
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 5 })
        
        [<Fact>]
        let ``Vertical: cascade stack with space available to N`` () =
            // ##......##
            // ##.[]...##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let expected = HashSet<Loc>(
                [ { Row = 2; Col = 4 }; { Row = 2; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
        
        [<Fact>]
        let ``Vertical: cascade stack with wall to N`` () =
            // ##########
            // ##.[]...##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse N { Row = 2; Col = 5 })
        
        [<Fact>]
        let ``Vertical: cascade stack with space available to S`` () =
            // ##..[]..##
            // ##.[]...##
            // ##......##
            // ##########
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall ]
                ]
            let expected = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 } ]) |> Some

            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 5 })
        
        [<Fact>]
        let ``Vertical: cascade stack with wall to S`` () =
            // ##..[]..##
            // ##.[]...##
            // ##########
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall ]
                ]
            let expected = None

            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 4 })
            Assert.Equivalent(expected, findEmpty warehouse S { Row = 0; Col = 5 })

    module PushBoxes =
    
        [<Fact>]
        let ``Horizontal: next tile is empty E`` () =
            let input = loadInput "#.O.#\r\n\r\n>>" // ##..[]..##
            let warehouse1, _ = parse input
            let warehouse2 = Array2D.copy warehouse1
            let tiles = HashSet<Loc>([ { Row = 0; Col = 4 }; { Row = 0; Col = 5 } ])
            let expected = [ Empty; BoxLeft; BoxRight; Empty; ]

            pushBoxes warehouse1 tiles E
            pushBoxes warehouse2 tiles W
            
            Assert.Equal(expected, warehouse1[0,4..7])
            Assert.Equal(expected, warehouse2[0,2..5])
        
        [<Fact>]
        let ``Horizontal: empty tile is past multiple boxes tiles away empty`` () =
            let input = loadInput "#.OOO.#\r\n\r\n>" // ##..[][][]..##
            let warehouse1, _ = parse input
            let warehouse2 = Array2D.copy warehouse1
            let tiles = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 0; Col = 6 }; { Row = 0; Col = 7 }
                  { Row = 0; Col = 8 }; { Row = 0; Col = 9 } ])
            let expected = [ Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; ]

            pushBoxes warehouse1 tiles E
            pushBoxes warehouse2 tiles W

            Assert.Equal(expected, warehouse1[0,4..11])
            Assert.Equal(expected, warehouse2[0,2..9])
            
        [<Fact>]
        let ``Vertical: single box with space available`` () =
            // ##......##
            // ##..[]..##
            // ##......##
            let input = loadInput "#...#\r\n#.O.#\r\n#...#\r\n\r\n>"
            let warehouse1, _ = parse input
            let warehouse2 = Array2D.copy warehouse1
            let tiles = HashSet<Loc>([ { Row = 1; Col = 4 }; { Row = 1; Col = 5 } ])

            let expected1 = array2D [[Empty; BoxLeft; BoxRight; Empty; ]; [ Empty; Empty; Empty; Empty ]]
            let expected2 = array2D [[ Empty; Empty; Empty; Empty ]; [Empty; BoxLeft; BoxRight; Empty; ]]
            
            pushBoxes warehouse1 tiles N
            pushBoxes warehouse2 tiles S

            Assert.Equal(expected1, warehouse1[0..1,3..6])
            Assert.Equal(expected2, warehouse2[1..2,3..6])
                            
        [<Fact>]
        let ``Vertical: stacked boxes with space available`` () =
            // ##......##
            // ##..[]..##
            // ##..[]..##
            // ##......##
            let input = loadInput "#...#\r\n#.O.#\r\n#.O.#\r\n#...#\r\n\r\n>"
            let warehouse1, _ = parse input
            let warehouse2 = Array2D.copy warehouse1
            let tiles = HashSet<Loc>(
                [ { Row = 1; Col = 4 }; { Row = 1; Col = 5 }
                  { Row = 2; Col = 4 }; { Row = 2; Col = 5 } ])

            let expected1 = array2D [
                [ Empty; BoxLeft; BoxRight; Empty; ]
                [ Empty; BoxLeft; BoxRight; Empty; ]
                [ Empty; Empty; Empty; Empty ]
            ]
            let expected2 = array2D [
                [ Empty; Empty; Empty; Empty ]
                [ Empty; BoxLeft; BoxRight; Empty; ]
                [ Empty; BoxLeft; BoxRight; Empty; ]
            ]
            
            pushBoxes warehouse1 tiles N
            pushBoxes warehouse2 tiles S

            Assert.Equal(expected1, warehouse1[0..2,3..6])
            Assert.Equal(expected2, warehouse2[1..3,3..6])
                    
        [<Fact>]
        let ``Vertical: double stack with space available to N`` () =
            // ##......##
            // ##.[][].##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let tiles = HashSet<Loc>(
                [ { Row = 2; Col = 4 }; { Row = 2; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 }
                  { Row = 1; Col = 5 }; {Row = 1; Col = 6 } ])

            pushBoxes warehouse tiles N
        
        [<Fact>]
        let ``Vertical: double stack with space available to S`` () =
            // ##..[]..##
            // ##.[][].##
            // ##......##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; BoxLeft; BoxRight; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                ]
            let tiles = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 }
                  { Row = 1; Col = 5 }; {Row = 1; Col = 6 } ])

            pushBoxes warehouse tiles S
        
        [<Fact>]
        let ``Vertical: cascade stack with space available to N`` () =
            // ##......##
            // ##.[]...##
            // ##..[]..##
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                ]
            let tiles = HashSet<Loc>(
                [ { Row = 2; Col = 4 }; { Row = 2; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 } ])

            pushBoxes warehouse tiles N
        
        [<Fact>]
        let ``Vertical: cascade stack with space available to S`` () =
            // ##..[]..##
            // ##.[]...##
            // ##......##
            // ########## // TODO: why is this needed?  Is that a problem?
            let warehouse =
                array2D [
                    [ Wall; Wall; Empty; Empty; BoxLeft; BoxRight; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; BoxLeft; BoxRight; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Empty; Empty; Empty; Empty; Empty; Empty; Wall; Wall ]
                    [ Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall ]
                ]
            let tiles = HashSet<Loc>(
                [ { Row = 0; Col = 4 }; { Row = 0; Col = 5 }
                  { Row = 1; Col = 3 }; { Row = 1; Col = 4 } ])

            pushBoxes warehouse tiles S

    module Solve =

        [<Fact>]
        let ``part2: large sample`` () =
            let input = loadInput sample2
            let expected = 9021

            Assert.Equal(expected, execute input)
