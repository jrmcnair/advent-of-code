module Tests.Day10

open System
open Xunit
open Day10

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

[<Fact>]
let ``toGrid: 2x2`` () =
    let input = loadInput("10\r\n21")
    let expected = array2D [ [Height 1; Trailhead]; [Height 2; Height 1] ]
    
    Assert.Equal(expected, toGrid input)

[<Fact>]
let ``toGrid: 3x3 - all node types`` () =
    let input = loadInput(".01\r\n210\r\n32.")
    let expected = array2D [
        [Impassable; Trailhead; Height 1]
        [Height 2; Height 1; Trailhead]
        [Height 3; Height 2; Impassable]
    ]
    
    Assert.Equal(expected, toGrid input)

[<Fact>]
let ``getTrailheads: 1`` () =
    let grid = loadInput("10\r\n21") |> toGrid
    let expected = [ (0, 1) ]
    
    Assert.Equal(expected, getTrailheads grid)

[<Fact>]
let ``getTrailheads: 2`` () =
    let grid = loadInput(".01\r\n210\r\n02.") |> toGrid
    let expected = [ (0, 1); (1, 2); (2, 0) ]
    
    Assert.Equal(expected, getTrailheads grid)

[<Fact>]
let ``getPeakCount: single peak`` () =
    let input = loadInput("0123
1234
8765
9876")
    let grid = input |> toGrid
    let actual = getPeakCount grid (0, 0)

    Assert.Equivalent(1, actual)

[<Fact>]
let ``getPeakCount: two peaks`` () =
    let input = loadInput("...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")
    let grid = input |> toGrid
    let actual = getPeakCount grid (0, 3)

    Assert.Equivalent(2, actual)

[<Fact>]
let ``part1: score of 0 (no trailheads)`` () =
    let input = loadInput(".123
1234
8765
9876")
    let grid = input |> toGrid

    Assert.Equal(0, part1 grid)

[<Fact>]
let ``part1: score of 0 (no peaks)`` () =
    let input = loadInput("0123
1234
8765
.876")
    let grid = input |> toGrid

    Assert.Equal(0, part1 grid)

[<Fact>]
let ``part1: score of 1 (single trailhead)`` () =
    let input = loadInput("0123
1234
8765
9876")
    let grid = input |> toGrid

    Assert.Equal(1, part1 grid)

[<Fact>]
let ``part1: score of 2 (single trailhead)`` () =
    let input = loadInput("...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")
    let grid = input |> toGrid

    Assert.Equal(2, part1 grid)

[<Fact>]
let ``part1: score of 4 (single trailhead)`` () =
    let input = loadInput("..90..9
...1.98
...2..7
6543456
765.987
876....
987....")
    let grid = input |> toGrid

    Assert.Equal(4, part1 grid)

[<Fact>]
let ``part1: score of 3 (two trailheads)`` () =
    let input = loadInput("10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")
    let grid = input |> toGrid

    Assert.Equal(3, part1 grid)

[<Fact>]
let ``part1: sample`` () =
    let input = loadInput("89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")
    let grid = input |> toGrid

    Assert.Equal(36, part1 grid)

[<Fact>]
let ``part2: single trailhead = 3`` () =
    let input = loadInput(".....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....")
    let grid = input |> toGrid

    Assert.Equal(3, part2 grid)

[<Fact>]
let ``part2: single trailhead = 13`` () =
    let input = loadInput("..90..9
...1.98
...2..7
6543456
765.987
876....
987....")
    let grid = input |> toGrid

    Assert.Equal(13, part2 grid)

[<Fact>]
let ``part2: single trailhead = all!`` () =
    let input = loadInput("012345
123456
234567
345678
4.6789
56789.")
    let grid = input |> toGrid

    Assert.Equal(227, part2 grid)

[<Fact>]
let ``part2: sample`` () =
    let input = loadInput("89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")
    let grid = input |> toGrid

    Assert.Equal(81, part2 grid)

