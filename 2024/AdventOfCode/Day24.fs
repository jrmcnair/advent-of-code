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

let solve (wireState: Dictionary<string, int>) (gates: List<Gate>) =
    // TODO: change this to a rec function
    // TODO: will we ever have some that don't get executed?
    while gates.Count > 0 do
        let next = gates.Find (fun gate ->
            let in1, in2 = gate.Inputs
            wireState.ContainsKey(in1)
            && wireState.ContainsKey(in2)
            && not (wireState.ContainsKey(gate.Output)))
        updateState wireState next
        gates.Remove(next) |> ignore

    let bits =
        wireState
        |> List.ofSeq
        |> List.filter _.Key.StartsWith("z")
        |> List.sortByDescending _.Key // TODO: will this work for double digit Zs?
        |> List.map (fun kvp -> kvp.Value |>  string)
        |> String.concat ""
    Convert.ToInt64(bits, 2)

let part1 () =
    let wireState, gates = parse input

    solve wireState gates
    |> printfn "[Day 24] Part 1: %d" // 42410633905894

let part2 () =
    let wireState, gates = parse input

    0
    |> printfn "[Day 24] Part 2: %d"
