module Tests.Day24

open System.Collections.Generic
open Xunit
open Day24
open AdventOfCode.Tests.Common

let sample1 = "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"

let sample2 = "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"

[<Fact>]
let ``parse sample1`` () =
    let input = sample1
    let expectedState = dict [ "x00", 1; "x01", 1; "x02", 1; "y00", 0; "y01", 1; "y02", 0 ]
    let expectedGates = [
        { Type = AND; Inputs = ("x00", "y00"); Output = "z00" }
        { Type = XOR; Inputs = ("x01", "y01"); Output = "z01" }
        { Type = OR; Inputs = ("x02", "y02"); Output = "z02" }
    ]
    
    let actual = parse input

    Assert.Equivalent(expectedState, fst actual)
    Assert.Equivalent(expectedGates, snd actual)

[<Theory>]
[<InlineData(1, 1, 1)>]
[<InlineData(0, 1, 0)>]
[<InlineData(1, 0, 0)>]
[<InlineData(0, 0, 0)>]
let ``logicGate with AND operations`` (in1: int, in2: int, expected: int) =
    Assert.Equal(expected, logicGate (in1, in2) AND)

[<Theory>]
[<InlineData(1, 1, 1)>]
[<InlineData(0, 1, 1)>]
[<InlineData(1, 0, 1)>]
[<InlineData(0, 0, 0)>]
let ``logicGate with OR operations`` (in1: int, in2: int, expected: int) =
    Assert.Equal(expected, logicGate (in1, in2) OR)

[<Theory>]
[<InlineData(1, 1, 0)>]
[<InlineData(0, 1, 1)>]
[<InlineData(1, 0, 1)>]
[<InlineData(0, 0, 0)>]
let ``logicGate with XOR operations`` (in1: int, in2: int, expected: int) =
    Assert.Equal(expected, logicGate (in1, in2) XOR)

[<Fact>]
let `` solve sample 1`` () =
    let wireState, gates = parse sample1
    let expected = 4L
    
    Assert.Equal(expected, solve wireState gates)

[<Fact>]
let `` solve sample 2`` () =
    let wireState, gates = parse sample2
    let expected = 2024L
    
    Assert.Equal(expected, solve wireState gates)

