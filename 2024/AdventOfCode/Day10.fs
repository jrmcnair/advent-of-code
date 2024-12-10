module Day10

// TODO: do we need this?
type Coord = {
    Row: int
    Col: int
}

type Node =
    | Height of int
    | Impassable
    | Peak
    | Trailhead
module Node =
    let ofChar = function
        | '.' -> Impassable
        | '0' -> Trailhead
        | '9' -> Peak
        | x -> x |> string |> int |> Height
    let addRise  = function
        | Trailhead -> Height 1
        | Height 8 -> Peak
        | Height x -> Height (x + 1)
        | _ -> Impassable

let toGrid (input: string seq) =
    input
    |> Seq.map (Seq.map Node.ofChar)
    |> array2D

let getTrailheads (grid: Node[,]) =
    seq {
        for r in 0..(grid.GetLength 0 - 1) do
            for c in 0..(grid.GetLength 1 - 1) do
                if grid.[r,c] = Trailhead 
                    then yield { Row = r; Col = c }
    }

let getPeakCount (grid: Node[,]) (trailhead: Coord) =
    let maxRow, maxCol = (grid.GetLength 0) - 1, (grid.GetLength 1) - 1
    let getNeighboringNodes (c: Coord) (target: Node) =
        seq {
            if c.Row > 0 && grid[c.Row - 1, c.Col] = target then yield { Row = c.Row - 1; Col = c.Col }
            if c.Row < maxRow && grid[c.Row + 1, c.Col] = target then yield { Row = c.Row + 1; Col = c.Col }
            if c.Col > 0 && grid[c.Row, c.Col - 1] = target then yield { Row = c.Row; Col = c.Col - 1 }
            if c.Col < maxCol && grid[c.Row, c.Col + 1] = target then yield { Row = c.Row; Col = c.Col + 1 }
        }
            
    let rec walkTrail (coord: Coord) (peaks: Set<Coord>) =
        let current = grid[coord.Row, coord.Col]
        match current with
        | Peak -> peaks.Add coord
        | Trailhead
        | Height _ ->
            let targetNode = Node.addRise current
            getNeighboringNodes coord targetNode
            |> Seq.map (fun next ->
                walkTrail next peaks)
            |> Set.unionMany
        | _ -> peaks

    let result = walkTrail trailhead Set.empty
    result |> Set.count

let part1 (input:string seq) =
    let grid = input |> toGrid
    
    getTrailheads grid
    |> Seq.map (getPeakCount grid)
    |> Seq.sum

let part2 (input:string seq) = 0
