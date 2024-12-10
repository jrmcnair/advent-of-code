module Day10

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
                    then yield (r, c)
    }

let findPeaks (grid: Node[,]) (trailhead: int * int) =
    let maxRow, maxCol = (grid.GetLength 0) - 1, (grid.GetLength 1) - 1
    let getNeighbors (r: int, c: int) (target: Node) =
        seq {
            if r > 0 && grid[r - 1, c] = target then yield (r - 1, c)
            if r < maxRow && grid[r + 1, c] = target then yield (r + 1, c)
            if c > 0 && grid[r, c - 1] = target then yield (r, c - 1)
            if c < maxCol && grid[r, c + 1] = target then yield (r, c + 1)
        }

    let rec walkTrail (r:int, c:int) (peaks: (int * int) seq) =
        let current = grid[r, c]
        match current with
        | Peak -> Seq.append peaks [ (r, c) ]
        | Trailhead
        | Height _ ->
            let targetNode = Node.addRise current
            getNeighbors (r, c) targetNode
            |> Seq.map (fun next ->
                walkTrail next peaks)
            |> Seq.concat
        | _ -> peaks

    walkTrail trailhead Seq.empty

let getPeakCount (grid: Node[,]) (trailhead: int * int) =
    findPeaks grid trailhead
    |> Seq.distinct
    |> Seq.length

let getTrailCount (grid: Node[,]) (trailhead: int * int) =
    findPeaks grid trailhead
    |> Seq.length

let part1 (input:string seq) =
    let grid = input |> toGrid
    
    getTrailheads grid
    |> Seq.map (getPeakCount grid)
    |> Seq.sum

let part2 (input:string seq) =
    let grid = input |> toGrid
    
    getTrailheads grid
    |> Seq.map (getTrailCount grid)
    |> Seq.sum
