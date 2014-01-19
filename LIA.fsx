#r "Microsoft.Z3.dll"
open Microsoft.Z3

let ctx = new Context()
let x = ctx.MkIntConst("x")
let y = ctx.MkIntConst("y")
let p1 = ctx.MkGe(x, ctx.MkInt 0) // x >= 0
let p2 = ctx.MkEq(y, ctx.MkAdd(x, ctx.MkInt 1)) // y = x + 1
let p34 = ctx.MkOr(ctx.MkGt(y, ctx.MkInt 2), ctx.MkLt(y, ctx.MkInt 1)) // y > 2 v y < 1

let solver = ctx.MkSolver()
solver.Assert(p1, p2, p34);;
match solver.Check() with
| Status.SATISFIABLE -> printfn "Result:\n %A" solver.Model
| _ -> printfn "No solution"