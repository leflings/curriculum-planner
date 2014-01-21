module Scheduler

open System
open Types
open Utils
open Microsoft.Z3


module Helpers =
    let ctx = new Context()
    let is = ctx.MkIntSort() :> Sort
    let bs = ctx.MkBoolSort() :> Sort

    // Define datatype
    let cCons = ctx.MkConstructor("Course", "mk-course", [|"incl";"number";"code";"ects"|], [|bs; is; is; is|])
    let cSort = ctx.MkDatatypeSort("Course", [|cCons|]) :> Sort

    // Define datatype accessors
    let incl c = cCons.AccessorDecls.[0].Apply([|c|]) :?> BoolExpr
    let number c = cCons.AccessorDecls.[1].Apply([|c|]) :?> ArithExpr
    let code c = cCons.AccessorDecls.[2].Apply([|c|]) :?> ArithExpr
    let ects c = cCons.AccessorDecls.[3].Apply([|c|]) :?> ArithExpr

    // Define helper macros (is what declare-fun in SMT would do anyway; expand the macro)
    let minimum e1 e2 = ctx.MkITE(ctx.MkLe(e1,e2),e1,e2) :?> ArithExpr
    let maximum e1 e2 = ctx.MkITE(ctx.MkGe(e1,e2),e1,e2) :?> ArithExpr
    let absolute e = ctx.MkITE(ctx.MkGe(e,ctx.MkInt(0)),e,ctx.MkUnaryMinus(e)) :?> ArithExpr
    let ectsContribution e = ctx.MkITE( incl e, ects e, ctx.MkInt(0) ) :?> ArithExpr 
    let checkCode c1 c2 =
        ctx.MkAnd([|ctx.MkDistinct([|c1 :> Expr; c2 :> Expr|]);
                    ctx.MkDistinct([|ctx.MkInt(10) :> Expr;
                                    absolute ( ctx.MkDiv( maximum c1 c2, minimum c1 c2 ) ) :> Expr |] ) |] )


    // Define auxillery functions for setting assertions
    let courses (zs : ZCourse list) = [| for z in zs -> ctx.MkConst(sprintf "C%d" z.ZNo, cSort) |]

    let setCourse course (zc : ZCourse) =
        let setField (course:Expr) (field : Expr -> ArithExpr) (i:int) = ctx.MkEq(ctx.MkInt(i), field course)
        let setter = setField course
        List.map2 (fun f v -> setter f v) [number; ects; code] [zc.ZNo; zc.ZECTS; zc.ZCode] |> List.toArray

    let setCourses (courses : Expr []) (cs : ZCourse list) = 
        let comb = Array.zip courses (Array.ofList cs)
        [| for (c,zc) in comb do yield! (setCourse c zc) |]

    let checkCourse c1 c2 = ctx.MkITE(
                                ctx.MkAnd([|incl c1; incl c2|]),
                                checkCode (code c1) (code c2),
                                ctx.MkTrue()) :?> BoolExpr

    let checkCourses (cs : Expr []) = 
        [| for i in 0..cs.Length-1 do
            for j in i+1..cs.Length-1 do
                yield checkCourse cs.[i] cs.[j] |]

    let checkConstraints (cs : Expr []) (constraints : int list) =
        [| for i in 0..cs.Length-1 do
            for j in constraints do
                yield ctx.MkITE(
                        incl cs.[i],
                        checkCode (code cs.[i]) (ctx.MkInt(j) :> ArithExpr),
                        ctx.MkTrue()) :?> BoolExpr |]

open Helpers
let schedule courselist (minECTS, maxECTS) constraints =
    let sum = ctx.MkIntConst("ectssum")
    let cs = courses courselist
    let solver = ctx.MkSolver()

    solver.Assert(setCourses cs courselist)
    solver.Assert(checkConstraints cs constraints)
    solver.Assert(checkCourses cs)
    solver.Assert(ctx.MkEq(sum, ctx.MkAdd(Array.map ectsContribution cs)))
    solver.Assert(ctx.MkAnd(ctx.MkLe(ctx.MkInt(toZECTS minECTS), sum), ctx.MkLe(sum, ctx.MkInt(toZECTS maxECTS))))

    let yielder _ =
        match solver.Check() with
        | Status.SATISFIABLE ->
            let m = solver.Model
            let chosen = [ for c in cs do
                            let b = m.Evaluate(incl c, true).IsTrue
                            let n = Convert.ToInt32(m.Evaluate(number c, true).ToString())
                            if b then yield sprintf "%05d" n else () ]
            let assignments = [| for c in cs -> ctx.MkEq(c, m.Evaluate(c, true)) |]
            solver.Assert(ctx.MkNot(ctx.MkAnd assignments))
            printfn "%A" chosen
            chosen
        | _ -> []
    Seq.cache (Seq.initInfinite yielder)