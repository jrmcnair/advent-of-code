module Day1

open System
open System.IO

let parseDataLine (line:string) : Tuple<int, int> =
    line.Split("   ")
    |> (fun x -> x.[0] |> int, x.[1] |> int)

let loadData dataFile : Tuple<int[], int[]> =
    dataFile
    |> File.ReadLines
    |> Seq.map parseDataLine
    |> Array.ofSeq
    |> Array.unzip

// -- part 1 -------------------------------------------------------------------

let totalDistance data =
    data
    |> fun (x, y) -> Array.sort x, Array.sort y
    ||> Array.zip
    |> Array.map (fun (x, y) -> abs(x - y))
    |> Array.sum

// -- part 2 -------------------------------------------------------------------

let computeSimilarity (list: int[]) (num: int) =
    list
    |> Array.filter (fun x -> x = num)
    |> (fun matchingIds -> (matchingIds |> Array.length) * num)

let computeSimilarities (lists: Tuple<int[], int[]>) : int[] =
    let list1, list2 = lists
    list1 |> Array.map (computeSimilarity list2)

let similarityScore data =
    data
    |> computeSimilarities
    |> Array.sum

let run () =
    let data = loadData "./Input/day1.txt"

    totalDistance data
    |> printfn "[Day 1] Part 1: %d"

    similarityScore data
    |> printfn "[Day 1] Part 2: %d"
