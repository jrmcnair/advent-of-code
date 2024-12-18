// https://adventofcode.com/2024/day/17
module Day17

open System.Collections.Generic
open System.Text.RegularExpressions

type Registers = { A: int64; B: int64; C: int64 } // int64?
type Program = (Instruction * Operand) array
and Instruction = | ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
and Operand = | Literal of int | Combo of int
and InstructionResult = {
    Registers: Registers
    Output: string option
    Jump: int option
}

let parse (input: string) =
    let toInstruction = function
        | 0 -> ADV | 1 -> BXL | 2 -> BST | 3 -> JNZ
        | 4 -> BXC | 5 -> OUT | 6 -> BDV | _ -> CDV

    let toOperand (instruction: Instruction) (num: int) =
        match instruction with
        | ADV | OUT | BST | BDV | CDV -> Combo num
        | BXL | JNZ | BXC-> Literal num

    Regex.Matches(input, "(\d+),(\d+)")
    |> Seq.map (fun m ->
        let instruction = m.Groups[1].Value |> int |> toInstruction
        instruction, m.Groups[2].Value |> int |> toOperand instruction)
    |> Array.ofSeq

let comboValue (registers: Registers)= function
    | x when x >= 0 && x <= 3 -> int64 x
    | 4 -> registers.A
    | 5 -> registers.B
    | 6 -> registers.C
    | _ -> failwith "invalid input"

let rec pow (baseNumber: int64) (exponent: int64) =
    if exponent = 0L then 1L
    else baseNumber * (pow baseNumber (exponent - 1L))

let processInstruction (registers: Registers) = function
    | ADV, Combo n ->
        let combo = comboValue registers n
        { Registers = { registers with A = registers.A / (pow 2L combo) }
          Output = None
          Jump = None }
    | BXL, Literal n ->         
        { Registers = { registers with B = registers.B ^^^ n }
          Output = None
          Jump = None }
    | BST, Combo n ->
        let combo = comboValue registers n
        { Registers = { registers with B = combo % 8L }
          Output = None
          Jump = None }
    | JNZ, Literal n ->
        if registers.A = 0 then { Registers = registers; Output = None; Jump = None }
        else { Registers = registers; Output = None; Jump = Some n }
    | BXC, Literal _ ->
        { Registers = { registers with B = registers.B ^^^ registers.C }
          Output = None
          Jump = None }
    | OUT, Combo n ->
        { Registers = registers
          Output = (comboValue registers n) % 8L |> string |> Some
          Jump = None }
    | BDV, Combo n ->
        let combo = comboValue registers n
        { Registers = { registers with B = registers.A / (pow 2L combo) }
          Output = None
          Jump = None }
    | CDV, Combo n ->
        let combo = comboValue registers n
        { Registers = { registers with C = registers.A / (pow 2L combo) }
          Output = None
          Jump = None }
    | _ -> failwith "invalid input"

let solve (program: Program) (startingRegisters: Registers) =
    let mutable i = 0
    let mutable registers = startingRegisters
    let output = List<string>()
    
    while i < program.Length do
        let result = processInstruction registers program[i]
        if result.Jump |> Option.isSome then i <- result.Jump.Value
        else
            if result.Output |> Option.isSome then output.Add(result.Output.Value)
            registers <- result.Registers
            i <- i + 1

    output |> String.concat ","

let part2 (input: string) =
    let program = parse input
    let reversed = input.Split(',') |> Array.rev
    let mutable validAs = Set<int64>([ 0L ])

    for target in reversed do
        let nextValuesForA = HashSet<int64>()
        for a in validAs do
            let shifted = a * 8L // free up last 3 bits for next attempts

            for candidate in [shifted..shifted + 7L] do // test all possibilities for last 3 bits
                let output = solve program { A = candidate; B = 0; C = 0 }
                if output[0] = char target then nextValuesForA.Add(candidate) |> ignore

        validAs <- Set(nextValuesForA)

    validAs |> Seq.min

let run () =
    let registers = { A = 52884621; B = 0; C = 0 }
    let input = "2,4,1,3,7,5,4,7,0,3,1,5,5,5,3,0"
    let program = parse input

    solve program registers
    |> printfn "[Day 17] Part 1: %s"
    
    part2 input
    |> printfn "[Day 17] Part 2: %d"
