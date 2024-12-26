open System
open Day24

let startTime = DateTime.Now

part1()
let part1Time = DateTime.Now
printfn $"[{part1Time}] Completed in {(part1Time - startTime).Milliseconds}ms"

part2()
let part2Time = DateTime.Now
printfn $"[{part2Time}] Completed in {(part2Time - part1Time).Milliseconds}ms"
