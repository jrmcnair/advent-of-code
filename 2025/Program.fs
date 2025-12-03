open System
open AdventOfCode2025

let execute func =
    let startTime = DateTime.Now

    func ()

    let endTime = DateTime.Now
    printfn $"[{endTime}] Completed in {(endTime - startTime).Milliseconds}ms"

execute Day3.Part1.run
execute Day3.Part2.run
