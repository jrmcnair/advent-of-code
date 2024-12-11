module Day2

open System.IO

let parseDataLine (line:string) =
    line.Split(" ") |> Seq.map (int) |> List.ofSeq

let loadData dataFile =
    dataFile
    |> File.ReadLines
    |> Seq.map parseDataLine

let allInRange =
    List.forall (fun (x, y) -> abs(x - y) <= 3)

let allIncreasing  =
    List.forall (fun (x, y) -> x < y)
    
let allDecreasing  =
    List.forall (fun (x, y) -> x > y)

// -- part 1 ------------------------------------------------------------------- 

let isReportSafe (report: int list) =
    let pairs = report |> List.pairwise

    allInRange pairs
    && (allIncreasing pairs || allDecreasing pairs)

let safeReportCount data =
    data
    |> Seq.filter (isReportSafe)
    |> Seq.length

// -- part 2 ------------------------------------------------------------------- 

let isReportSafeWithDampener (report: int list) =
    isReportSafe report
    ||
        report
        |> List.mapi (fun i _ ->
            report
            |> List.removeAt i
            |> isReportSafe )
        |> List.exists id

let safeReportCountWithDampener data =
    data
    |> Seq.filter (isReportSafeWithDampener)
    |> Seq.length

let run () =
    let data = loadData "./Input/day2.txt"

    safeReportCount data
    |> printfn "[Day 2] Part 1: %d"

    safeReportCountWithDampener data
    |> printfn "[Day 2] Part 2: %d"
