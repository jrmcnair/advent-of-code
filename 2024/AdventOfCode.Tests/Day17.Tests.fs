module Tests.Day17

open Xunit
open Day17

let sampleRegisters = { A = 729; B = 0; C = 0 }
let sampleProgram = parse "0,1,5,4,3,0"

[<Fact>]
let ``parse of each instruction is correct`` () =
    let expected = [|
        (ADV, Combo 7); (BXL, Literal 6); (BST, Combo 5); (JNZ, Literal 4)
        (BXC, Literal 3); (OUT, Combo 2); (BDV, Combo 1); (CDV, Combo 0)
    |]

    Assert.Equivalent(expected, parse "0,7,1,6,2,5,3,4,4,3,5,2,6,1,7,0")

[<Theory>]
[<InlineData(0,0)>]
[<InlineData(1,1)>]
[<InlineData(2,2)>]
[<InlineData(3,3)>]
[<InlineData(4,10)>]
[<InlineData(5,20)>]
[<InlineData(6,30)>]
let ``comboValue returns correct value for all valid inputs`` (input: int, expected: int64) =
    let registers = { A = 10L; B = 20L; C = 30L }
    Assert.Equal(expected, input |> comboValue registers)

module ProcessInstruction =

    [<Fact>]
    let ``process ADV Instruction`` () =
        let registers = { A = 24; B = 0; C = 0 }
        let expected =
            { Registers = { A = 3; B = 0; C = 0 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (ADV, Combo 3))
        
    [<Fact>]
    let ``process BXL Instruction`` () =
        let registers = { A = 0; B = 2; C = 0 }
        let expected =
            { Registers = { A = 0; B = 3; C = 0 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (BXL, Literal 1))
        
    [<Fact>]
    let ``process BST Instruction`` () =
        let registers = { A = 9; B = 0; C = 0 }
        let expected =
            { Registers = { A = 9; B = 1; C = 0 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (BST, Combo 4))
        
    [<Fact>]
    let ``process JNZ Instruction: A is 0`` () =
        let registers = { A = 0; B = 0; C = 0 }
        let expected =
            { Registers = registers
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (JNZ, Literal 2))
        
    [<Fact>]
    let ``process JNZ Instruction: A > 0`` () =
        let registers = { A = 3; B = 0; C = 0 }
        let expected =
            { Registers = registers
              Output = None; Jump = Some 5 }
        
        Assert.Equal(expected, processInstruction registers (JNZ, Literal 5))
        
    [<Fact>]
    let ``process BXC Instruction`` () =
        let registers = { A = 0; B = 0; C = 0 }
        let expected =
            { Registers = { A = 0; B = 2; C = 0 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (BST, Combo 2))
        
    [<Fact>]
    let ``process OUT Instruction`` () =
        let registers = { A = 0; B = 20; C = 0 }
        let expected =
            { Registers = { A = 0; B = 20; C = 0 }
              Output = Some "4"; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (OUT, Combo 5))

    [<Fact>]
    let ``process BDV Instruction`` () =
        let registers = { A = 35; B = 0; C = 4 }
        let expected =
            { Registers = { A = 35; B = 2; C = 4 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (BDV, Combo 6))

    [<Fact>]
    let ``process CDV Instruction`` () =
        let registers = { A = 27; B = 0; C = 0 }
        let expected =
            { Registers = { A = 27; B = 0; C = 6 }
              Output = None; Jump = None }
        
        Assert.Equal(expected, processInstruction registers (CDV, Combo 2))

[<Fact>]
let ``part1: sample`` () =
    let expected = "4,6,3,5,6,3,5,2,1,0"

    Assert.Equal(expected, solve sampleProgram sampleRegisters)

[<Fact>]
let ``part2: sample`` () =
    let input = "0,3,5,4,3,0"
    let expected = 117440L

    Assert.Equal(expected, part2 input)
