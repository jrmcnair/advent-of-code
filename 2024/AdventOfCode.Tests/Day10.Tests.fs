module Tests.Day10

open System
open Xunit
open Day10

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

[<Fact>]
let ``scratchpad`` () =
    let input = loadInput(".2.\r\n434\r\n...")
    let grid = input |> toGrid
    let c = { Row = 1; Col = 1 }
    let maxRow, maxCol = 2,2
    let target = Height 4
                
    // let up = if c.Row = 0 then None else Some grid[c.Row - 1, c.Col]
    // let down = if c.Row = maxRow then None else Some grid[c.Row + 1, c.Col]
    // let left = if c.Col = 0 then None else Some grid[c.Row, c.Col - 1]
    // let right = if c.Col = maxCol then None else Some grid[c.Row, c.Col + 1]

    let neighbors =
        seq {            
            if c.Row > 0 then yield ("up", { Row = c.Row - 1; Col = c.Col })
            if c.Row < maxRow then yield ("down", { Row = c.Row + 1; Col = c.Col })
            if c.Col > 0 then yield ("left", { Row = c.Row; Col = c.Col - 1 })
            if c.Col < maxCol then yield ("right", { Row = c.Row; Col = c.Col + 1 })
            // if c.Row > 0 && grid[c.Row - 1, c.Col] = target then yield { Row = c.Row - 1; Col = c.Col }
            // if c.Row < maxRow && grid[c.Row + 1, c.Col] = target then yield { Row = c.Row + 1; Col = c.Col }
            // if c.Col > 0 && grid[c.Row, c.Col - 1] = target then yield { Row = c.Row - 1; Col = c.Col }
            // if c.Col < maxCol && grid[c.Row, c.Col + 1] = target then yield { Row = c.Row + 1; Col = c.Col }
        }
    ()

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
    let expected = [ { Row = 0; Col = 1 } ]
    
    Assert.Equal(expected, getTrailheads grid)

[<Fact>]
let ``getTrailheads: 2`` () =
    let grid = loadInput(".01\r\n210\r\n02.") |> toGrid
    let expected = [ { Row = 0; Col = 1 }; { Row = 1; Col = 2 }; { Row = 2; Col = 0 } ]
    
    Assert.Equal(expected, getTrailheads grid)

[<Fact>]
let ``getPeakCount: single peak`` () =
    let input = loadInput("0123
1234
8765
9876")
    let grid = input |> toGrid
    let actual = getPeakCount grid { Row = 0; Col = 0 }

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
    let actual = getPeakCount grid { Row = 0; Col = 3 }

    Assert.Equivalent(2, actual)

[<Fact>]
let ``part1: score of 0 (no trailheads)`` () =
    let input = loadInput(".123
1234
8765
9876")
    Assert.Equal(0, part1 input)

[<Fact>]
let ``part1: score of 0 (no peaks)`` () =
    let input = loadInput("0123
1234
8765
.876")
    Assert.Equal(0, part1 input)

[<Fact>]
let ``part1: score of 1 (single trailhead)`` () =
    let input = loadInput("0123
1234
8765
9876")
    Assert.Equal(1, part1 input)

[<Fact>]
let ``part1: score of 2 (single trailhead)`` () =
    let input = loadInput("...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")

    Assert.Equal(2, part1 input)

[<Fact>]
let ``part1: score of 4 (single trailhead)`` () =
    let input = loadInput("..90..9
...1.98
...2..7
6543456
765.987
876....
987....")

    Assert.Equal(4, part1 input)

[<Fact>]
let ``part1: score of 3 (two trailheads)`` () =
    let input = loadInput("10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")

    Assert.Equal(3, part1 input)

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

    Assert.Equal(36, part1 input)

