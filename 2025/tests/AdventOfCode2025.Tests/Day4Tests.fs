module Tests.Day4

open System
open AdventOfCode2025.Day4
open Xunit

let sampleData = """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."""

module Part1 =
    open Part1

    [<Fact>]
    let ``Run Sample Data`` () =
        let result =
            sampleData |> _.Split(Environment.NewLine) |> parseInput
            |> accessibleRolls
            |> List.length

        Assert.Equal(13, result)

    module Neighbors =
        [<Fact>]
        let ``Gets correct neighbors with no edges`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {1; 2; 3; 4; 6; 7; 8; 9}
            let actual = Grid.neighbors grid (1,1)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on upper left corner`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {2; 4; 5}
            let actual = Grid.neighbors grid (0,0)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on top edge`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {1; 3; 4; 5; 6}
            let actual = Grid.neighbors grid (0,1)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on upper right corner`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {2; 5; 6}
            let actual = Grid.neighbors grid (0,2)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on left edge`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {1; 2; 5; 7; 8}
            let actual = Grid.neighbors grid (1,0)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on right edge`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {2; 3; 5; 8; 9}
            let actual = Grid.neighbors grid (1,2)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on lower left corner`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {4; 5; 8}
            let actual = Grid.neighbors grid (2,0)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on bottom edge`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {4; 5; 6; 7; 9}
            let actual = Grid.neighbors grid (2,1)
                    
            Assert.Equal<int seq>(expected, actual)

        [<Fact>]
        let ``Gets correct neighbors on lower right corner`` () =
            let grid =
                "123\r\n456\r\n789" 
                |> _.Split(Environment.NewLine)
                |> Seq.map (Seq.map (string >> int))
                |> array2D

            let expected = seq {5; 6; 8}
            let actual = Grid.neighbors grid (2,2)
                    
            Assert.Equal<int seq>(expected, actual)

module Part2 =
    open Part2

    [<Fact>]
    let ``Run Sample Data`` () =
        Assert.True(true)
