module Day4

open System
open System.IO

let loadData filename  =
    let rows =
        filename
        |> File.ReadLines
        |> Seq.map _.ToCharArray()
        |> Array.ofSeq

    let size = rows |> Array.length
    
    Array2D.init size size (fun i j -> rows.[i].[j])

// -- part 1 ------------------------------------------------------------------- 

let checkSquareV1 (grid: char[,]) (row: int) (col: int) =
    let size = grid |> Array2D.length1

    seq {
        if row-3 >= 0 then grid[row-3..row,col] |> Array.rev |>String //up
        if row+3 < size then grid[row..row+3,col] |> String //down
        if col+3 < size then grid[row,col..col+3] |> String //right
        if col-3 >= 0 then grid[row,col-3..col] |> Array.rev |> String //left
        if row-3 >= 0 && col+3 < size then [| grid[row,col]; grid[row-1,col+1]; grid[row-2,col+2]; grid[row-3,col+3] |] |> String //diag up right
        if row-3 >= 0 && col-3 >= 0 then [| grid[row,col]; grid[row-1,col-1]; grid[row-2,col-2]; grid[row-3,col-3] |] |> String //diag up left
        if row+3 < size && col+3 < size then [| grid[row,col]; grid[row+1,col+1]; grid[row+2,col+2]; grid[row+3,col+3] |] |> String //diag down right
        if row+3 < size && col-3 >= 0 then [| grid[row,col]; grid[row+1,col-1]; grid[row+2,col-2]; grid[row+3,col-3] |] |> String //diag down left
    }
    |> Seq.map (fun x -> if x = "XMAS" then 1 else 0)
    |> Seq.sum

let part1 grid =
    grid
    |> Array2D.mapi (fun row col letter ->
        if letter = 'X'
        then checkSquareV1 grid row col
        else 0
    )
    |> Seq.cast<int>
    |> Seq.sum

// -- part 2 ------------------------------------------------------------------- 

let checkSquareV2 (grid: char[,]) (row: int) (col: int) =
    let size = grid |> Array2D.length1
    
    if row - 1 < 0 || col - 1 < 0 || row + 1 >= size || col + 1 >= size
    then 0
    else
        seq {
            [| grid[row-1,col-1]; grid[row,col]; grid[row+1,col+1] |] |> String
            [| grid[row+1,col-1]; grid[row,col]; grid[row-1,col+1] |] |> String
        }
        |> Seq.forall (fun x -> x = "MAS" || x = "SAM")
        |> fun x -> if x then 1 else 0

let part2 grid =
    grid
    |> Array2D.mapi (fun row col letter ->
        if letter = 'A'
        then checkSquareV2 grid row col
        else 0
    )
    |> Seq.cast<int>
    |> Seq.sum

let run () =
    let grid = loadData "./Input/day4.txt"

    part1 grid
    |> printfn "[Day 4] Part 1: %d"

    part2 grid
    |> printfn "[Day 4] Part 2: %d"
