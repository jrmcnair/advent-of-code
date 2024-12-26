// https://adventofcode.com/2024/day/24
module Day24

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type GateType = | AND | OR | XOR
module GateType =
    let ofString = function
        | "AND" -> AND
        | "OR" -> OR
        | "XOR" -> XOR
        | g -> failwith $"invalid gate type '{g}'"

type Gate =
    { Type: GateType
      Inputs: string * string
      Output: string }
module Gate =
    let hasInput (input: string) (gate: Gate) = fst gate.Inputs = input || snd gate.Inputs = input
    // let hasXInput gate = (fst gate.Inputs).StartsWith("x") || (snd gate.Inputs).StartsWith("x")
    // let hasYInput gate = (fst gate.Inputs).StartsWith("y") || (snd gate.Inputs).StartsWith("y")
    //let hasZOutput gate = gate.Output.StartsWith("z")

let parse (input: string) =
    let wireState = Dictionary<string, int>()

    let split = input.Split($"{Environment.NewLine}{Environment.NewLine}")    

    Regex.Matches(split[0], "(\w{3}): (\d{1})")
    |> Seq.iter (fun m -> wireState.Add(m.Groups[1].Value, m.Groups[2].Value |> int))

    let gates =
        Regex.Matches(input, "(\w{3}) (AND|OR|XOR) (\w{3}) -> (\w{3})")
        |> Seq.map (fun m ->
          { Type = GateType.ofString m.Groups[2].Value
            Inputs = m.Groups[1].Value, m.Groups[3].Value
            Output = m.Groups[4].Value })
        |> List
        
    wireState, gates

let logicGate (in1: int, in2: int) = function
    | AND -> if in1 = 1 && in2 = 1 then 1 else 0
    | OR -> if in1 = 1 || in2 = 1 then 1 else 0
    | XOR -> if in1 = in2 then 0 else 1

let updateState (wireState: Dictionary<string, int>) (gate: Gate) =
    let in1, in2 = wireState[fst gate.Inputs], wireState[snd gate.Inputs]
    let state = logicGate (in1, in2) gate.Type

    if wireState.ContainsKey(gate.Output)
    then wireState[gate.Output] <- state
    else wireState.Add(gate.Output, state)

let input = File.ReadAllText "./Input/day24.txt"

let gateIsReady (wireState: Dictionary<string, int>) (gate: Gate) =
    let in1, in2 = gate.Inputs

    wireState.ContainsKey(in1)
    && wireState.ContainsKey(in2)
    && not (wireState.ContainsKey(gate.Output))

let run (wireState: Dictionary<string, int>) (gates: List<Gate>) =
    while gates.Count > 0 do
        let next = gates.Find (gateIsReady wireState)
        updateState wireState next
        gates.Remove(next) |> ignore

let toNumber (wireState: Dictionary<string, int>) (prefix: string) = 
    let bits =
        wireState
        |> List.ofSeq
        |> List.filter _.Key.StartsWith(prefix)
        |> List.sortByDescending _.Key // TODO: will this work for double digits?
        |> List.map (fun kvp -> kvp.Value |>  string)
        |> String.concat ""
    Convert.ToInt64(bits, 2)

// let halfAdders (gates: Gate list) () =
//     let xyGates = gates |> Seq.filter (fun g -> Gate.hasXInput g && Gate.hasYInput g)
//     let xors = xyGates |> Seq.filter (fun g -> g.Type = XOR)
//     let ands = xyGates |> Seq.filter (fun g -> g.Type = AND)
//
//     ()

let part1 () =
    let wireState, gates = parse input
    
    run wireState gates

    toNumber wireState "z"
    |> printfn "[Day 24] Part 1: %d" // 42410633905894

let findOutput (in1: string) (in2: string) (gateType: GateType) (gates: List<Gate>)=
    gates |> Seq.tryFind (fun g ->
        let gin1, gin2 = g.Inputs
        g.Type = gateType
        && (in1 = gin1 || in1 = gin2)
        && (in2 = gin1 || in2 = gin2))
    |> Option.map _.Output
    |> Option.defaultValue ""

let swap (swapped: HashSet<string>) (a: byref<string>) (b: byref<string>) =
    swapped.Add(a) |> ignore
    swapped.Add(b) |> ignore

    let temp = a
    a <- b; b <- temp

let part2 () =
    let _, gates = parse input
    let swapped = HashSet<string>()
    let mutable carry: string = ""
    
    for i in 0 .. 45 do
        let suffix = $"%02d{i}"
        let mutable r1: string = ""
        let mutable z1: string = ""
        let mutable c1: string = ""

        let mutable m1 = gates |> findOutput $"x{suffix}" $"y{suffix}" XOR
        let mutable n1 = gates |> findOutput $"x{suffix}" $"y{suffix}" AND

        if carry <> "" then
            r1 <- gates |> findOutput carry m1 AND
            if r1 = "" then
                swap swapped &m1 &n1
                r1 <- gates |> findOutput carry m1 AND

            z1 <- gates |> findOutput carry m1 XOR
            if m1.StartsWith("z") then swap swapped &m1 &z1
            if n1.StartsWith("z") then swap swapped &n1 &z1
            if r1.StartsWith("z") then swap swapped &r1 &z1
            
            c1 <- gates |> findOutput r1 n1 OR

        if c1.StartsWith("z") && c1 <> "z45" then swap swapped &c1 &z1     

        carry <- if carry = "" then n1 else c1

    swapped
    |> Seq.sort
    |> String.concat ","
    |> printfn "[Day 24] Part 2: %s"
