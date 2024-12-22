open System

let day = "21"
let startTime = DateTime.Now
printfn $"[{startTime}] Starting Day {day}..."

Day21.part1()
let part1Time = DateTime.Now
printfn $"[{part1Time}] Completed in {(part1Time - startTime).Milliseconds}ms"

Day21.part2()
let part2Time = DateTime.Now
printfn $"[{part2Time}] Completed in {(part2Time - part1Time).Milliseconds}ms"
