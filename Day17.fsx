#load "Helpers.fsx"
#r "nuget: Microsoft.Z3"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 17

let runProgram startA startB startC (program: int array) =
    let rec f output regA regB regC instPtr =
        let comboOp =
            function
            | 0 -> 0 | 1 -> 1 | 2 -> 2 | 3 -> 3
            | 4 -> regA | 5 -> regB | 6 -> regC
        if instPtr > program.Length - 1 then
            output
        else
            match program[instPtr] with
            | 0 -> 
                // The adv instruction (opcode 0) performs division.
                // The numerator is the value in the A register.
                // The denominator is found by raising 2 to the power of the instruction's combo operand.
                let res = (float regA) / Math.Pow(2., comboOp program[instPtr+1] |> float) |> int
                f output res regB regC (instPtr+2)
            | 1 -> 
                // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
                // then stores the result in register B.
                f output regA (regB ^^^ program[instPtr+1]) regC (instPtr+2)
            | 2 ->
                // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits),
                // then writes that value to the B register.
                f output regA ((comboOp program[instPtr+1]) % 8) regC (instPtr+2)
            | 3 ->
                // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, 
                // it jumps by setting the instruction pointer to the value of 
                // its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
                if regA = 0 then
                    f output regA regB regC (instPtr+2)
                else
                    f output regA regB regC program[instPtr+1]
            | 4 ->
                // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B.
                // (For legacy reasons, this instruction reads an operand but ignores it.)
                f output regA (regB ^^^ regC) regC (instPtr+2)
            | 5 ->
                // The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value.
                // (If a program outputs multiple values, they are separated by commas.)
                f (Array.append output [|((comboOp program[instPtr+1]) % 8)|]) regA regB regC (instPtr+2)
            | 6 ->
                // The bdv instruction (opcode 6) works exactly like the adv 
                // instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
                let res = (float regA) / Math.Pow(2., comboOp program[instPtr+1] |> float) |> int
                f output regA res regC (instPtr+2)
            | 7 ->
                // The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register.
                // (The numerator is still read from the A register.)
                let res = (float regA) / Math.Pow(2., comboOp program[instPtr+1] |> float) |> int
                f output regA regB res (instPtr+2)

    f Array.empty startA startB startC 0

// Example
runProgram 729 0 0 [|0;1;5;4;3;0|]

let program = data[4].Substring(9) |> Helpers.split "," |> Array.map int

let ans1 = runProgram 63281501 0 0 program |> (fun a -> String.Join(',',a))

ans1

/// Part 2

open Microsoft.Z3

// Example
runProgram 117440 0 0 [|0;3;5;4;3;0|] = [|0;3;5;4;3;0|]

(* Program: 2,4,
            1,5,
            7,5,
            4,5,
            0,3,
            1,6,
            5,5,
            3,0 *)
(*
regB = regA % 8
regB = regB ^^^ 5
regC = int (regA / (2^regB))
regB = regB ^^^ regC
regA = int (regA / 8)
regB = regB ^^^ 6
output regB = 2
go back to start
*)

let ctx = new Context()
let solver = ctx.MkOptimize()

let a = ctx.MkBVConst("a", 64u)
let mutable regA = a
let mutable i = 0
let mkBv var iter = ctx.MkBVConst(sprintf "%s_%i" var iter, 64u)
let mkConst (i: int) = ctx.MkBV(i, 64u)

for output in program do
    // regB = regA % 8
    let regB_0 = mkBv "b_0" i
    solver.Add(ctx.MkEq(regB_0, ctx.MkBVSMod(regA, mkConst 8)))
    // regB = regB ^^^ 5
    let regB_1 = mkBv "b_1" i
    solver.Add(ctx.MkEq(regB_1, ctx.MkBVXOR(regB_0, mkConst 5)))
    // regC = int (regA / (2^regB))
    let regC = mkBv "c" i
    solver.Add(ctx.MkEq(regC, ctx.MkBVLSHR(regA, regB_1)))
    // regB = regB ^^^ regC
    let regB_2 = mkBv "b_2" i
    solver.Add(ctx.MkEq(regB_2, ctx.MkBVXOR(regB_1, regC)))
    // regA = int (regA / (2^3))
    let regA_0 = mkBv "a_0" i
    solver.Add(ctx.MkEq(regA_0, ctx.MkBVLSHR(regA, mkConst 3)))
    // regB = regB ^^^ 6
    let regB_3 = mkBv "b_3" i
    solver.Add(ctx.MkEq(regB_3, ctx.MkBVXOR(regB_2, mkConst 6)))
    // output regB = program[i]
    solver.Add(ctx.MkEq(ctx.MkBVSMod(regB_3, mkConst 8), mkConst output))
    // next iter, switch the regA and increase iter
    i <- i + 1
    regA <- regA_0

solver.MkMinimize(a) // find the lowest possible value
solver.Check()

let ans2 = solver.Model.Evaluate(ctx.MkBV2Int(a, false)).SExpr() |> int64

ans2
