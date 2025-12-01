open System
open AdventOfCode2025.Day1

let startTime = DateTime.Now

Part1.run()

let part1Time = DateTime.Now
printfn $"[{part1Time}] Completed in {(part1Time - startTime).Milliseconds}ms"
