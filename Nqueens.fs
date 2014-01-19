/// <summary> An F# example using Z3 API, written by Anh-Dung Phan.
/// <para>This example explains how to generate Linear Integer Arithmetic constraints
/// for getting a solution and counting all solutions of N-queens problem. </para>
/// </summary>
module Nqueens

open System
open Microsoft.Z3

// All necessary APIs can be found at http://research.microsoft.com/en-us/um/redmond/projects/z3/class_microsoft_1_1_z3_1_1_context.html
// There is an online example in C# http://z3.codeplex.com/SourceControl/changeset/view/89c1785b7322#examples/dotnet/Program.cs
// which should be easy enough to adapt to F#

/// Use a global context for creating constraints
let ctx = new Context()

/// Create 'size' variables in Linear Integer Arithmetic
let queens size = 
    [| for i in 1..size ->
         ctx.MkIntConst(sprintf "Q_%i" i) |]

/// Each queen has to be in some row in the board
let rows (queens: _ []) size =
    [| for i in 0..size-1 ->
         ctx.MkAnd(ctx.MkGe(queens.[i], ctx.MkInt 0), ctx.MkLe(queens.[i], ctx.MkInt (size-1))) |]

/// Not any two queens are in the same row
let columns (queens: _ []) size = 
    let qs = [| for queen in queens -> queen :> Expr |]
    ctx.MkDistinct(qs)

/// Not any two queens are in the same forward or backward diagonal
let diagonals (queens: _ []) size = 
    [| for i in 1..size-1 do
         for j in 0..i-1 do
            let forward = ctx.MkDistinct(ctx.MkSub(queens.[i], queens.[j]), ctx.MkSub(ctx.MkInt i, ctx.MkInt j)) 
            let backward = ctx.MkDistinct(ctx.MkSub(queens.[i], queens.[j]), ctx.MkSub(ctx.MkInt j, ctx.MkInt i))
            yield ctx.MkAnd(forward, backward) |]

/// Find a solution for a board of size x size
let solve size =
    /// Create a solver based on the context. There might be multiple solvers per context.
    let solver = ctx.MkSolver()

    let qs = queens size
    solver.Assert(rows qs size)
    solver.Assert(columns qs size)
    solver.Assert(diagonals qs size)

    let result = solver.Check()
    if result = Status.SATISFIABLE then
        let m = solver.Model        
        let rows = [| for q in qs do
                        // Get the corresponding value in the satisfying assignment
                        let v = Convert.ToInt32(m.Evaluate(q, true).ToString()) 
                        yield Array.fold (fun acc x -> if x = v then acc + " X" else acc + " —") "" [|0..qs.Length-1|] |]
        printfn "Result for N = %i:" size
        Array.iter (printfn "%s") rows        
    else printfn "No solution"
    result

/// Count all solutions for a board size x size
let countSolutions size =
    let solver = ctx.MkSolver()
    
    let qs = queens size
    solver.Assert(rows qs size)
    solver.Assert(columns qs size)
    solver.Assert(diagonals qs size)

    /// Find a new satisfying assignment until unsatisfiable
    let rec loop count = 
        let result = solver.Check()
        if result = Status.SATISFIABLE then
            let m = solver.Model        
            let assignments = [| for q in qs -> ctx.MkEq(q, m.Evaluate(q, true)) |]
            solver.Assert(ctx.MkNot(ctx.MkAnd assignments)) // Ensure that this solution will not be found again
            loop (count + 1)
        else count

    loop 0