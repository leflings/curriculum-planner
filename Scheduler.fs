module Scheduler

open System
open Types
open Microsoft.Z3

let ctx = new Context()

let is = ctx.MkIntSort() :> Sort
let bs = ctx.MkBoolSort() :> Sort

let cCons =
    let fieldnames = ["incl";"number";"code";"ects"] |> List.toArray
    let sorts = [bs; is; is; is] |> List.toArray
    ctx.MkConstructor("Course", "mk-course", fieldnames, sorts)
let cSort = ctx.MkDatatypeSort("Course", [|cCons|]) :> Sort
let cIncl = cCons.AccessorDecls.[0]
let cNumber = cCons.AccessorDecls.[1]
let cCode = cCons.AccessorDecls.[2]
let cEcts = cCons.AccessorDecls.[3]

let incl c = cIncl.Apply([|c|]) :?> BoolExpr
let number c = ctx.MkApp(cNumber,[|c|]) :?> ArithExpr
let code c = ctx.MkApp(cCode,[|c|]) :?> ArithExpr
let ects c = ctx.MkApp(cEcts,[|c|]) :?> ArithExpr

let minimum e1 e2 = ctx.MkITE(ctx.MkLe(e1,e2),e1,e2) :?> ArithExpr
let maximum e1 e2 = ctx.MkITE(ctx.MkGe(e1,e2),e1,e2) :?> ArithExpr
let absolute e = ctx.MkITE(ctx.MkGe(e,ctx.MkInt(0)),e,ctx.MkUnaryMinus(e)) :?> ArithExpr

let ectsContribution e =
    ctx.MkITE(
        incl e,
        ects e,
        ctx.MkInt(0)
    ) :?> ArithExpr

let checkCode c1 c2 =
    ctx.MkAnd(
        [|
            ctx.MkDistinct([|c1 :> Expr; c2 :> Expr|]);
            ctx.MkDistinct(
                [|ctx.MkInt(10) :> Expr;
                    absolute (
                        ctx.MkDiv(
                            maximum c1 c2,
                            minimum c1 c2
                        )
                    ) :> Expr
                |]
            )
        |]
    )

let checkCourse e1 e2 =
    let i1 = incl e1
    let i2 = incl e2
    let (c1,c2) = (code e1, code e2)
    ctx.MkITE(
        ctx.MkAnd([|i1;i2|]),
        checkCode c1 c2,
        ctx.MkTrue()) :?> BoolExpr

let setCourse course (zc : ZCourse) =
    let setField (course:Expr) (field : Expr -> ArithExpr) (i:int) = ctx.MkEq(ctx.MkInt(i), field course)
    let setter = setField course
    List.map2 (fun f v -> setter f v) [number; ects; code] [zc.ZNo; zc.ZECTS; zc.ZCode] |> List.toArray

let courses (cs : ZCourse list) =
    [| for c in cs ->
        ctx.MkConst(sprintf "C%d" c.ZNo, cSort) |]

let setCourses (courses : Expr []) (cs : ZCourse list) = 
    let comb = Array.zip courses (Array.ofList cs)
    [| for (c,zc) in comb do
        yield! (setCourse c zc) |]

let checkCourses (cs : Expr []) =
    [| for i in 0..cs.Length-1 do
        for j in i+1..cs.Length-1 do
            yield checkCourse cs.[i] cs.[j] |]

let checkConstraints (cs : Expr []) (constraints : int list) =
    [| for i in 0..cs.Length-1 do
        for j in constraints do
            yield ctx.MkITE(incl cs.[i], checkCode (code cs.[i]) (ctx.MkInt(j) :> ArithExpr), ctx.MkTrue()) :?> BoolExpr |]

let schedule courselist constraints =
    let sum = ctx.MkIntConst("ectssum")
    let courses = courses courselist
    let solver = ctx.MkSolver()
    let sets = setCourses courses courselist
    let constraintchecks = checkConstraints courses constraints
    let coursechecks = checkCourses courses

    solver.Assert(sets)
    solver.Assert(constraintchecks)
    solver.Assert(coursechecks)
    solver.Assert(ctx.MkEq(sum, ctx.MkAdd(Array.map ectsContribution courses)))
    solver.Assert(ctx.MkAnd(ctx.MkLe(ctx.MkInt(10), sum), ctx.MkLe(sum, ctx.MkInt(12))))

    match solver.Check() with
    | Status.SATISFIABLE ->
        let m = solver.Model
        let chosen = [ for c in courses do
                        let b = m.Evaluate(incl c, true).IsTrue
                        let n = Convert.ToInt32(m.Evaluate(number c, true).ToString())
                        if b then yield sprintf "%05d" n else () ]
//        let assignments = [| for c in courses -> ctx.MkEq(c, m.Evaluate(c, true)) |]
//        solver.Assert(ctx.MkNot(ctx.MkAnd assignments))
        chosen
    | _ -> []

let countSolutions courselist constraints =
    let sum = ctx.MkIntConst("ectssum")
    let courses = courses courselist
    let solver = ctx.MkSolver()
    let sets = setCourses courses courselist
    let constraintchecks = checkConstraints courses constraints
    let coursechecks = checkCourses courses

    solver.Assert(sets)
    solver.Assert(constraintchecks)
    solver.Assert(coursechecks)
    solver.Assert(ctx.MkEq(sum, ctx.MkAdd(Array.map ectsContribution courses)))
    solver.Assert(ctx.MkAnd(ctx.MkLe(ctx.MkInt(10), sum), ctx.MkLe(sum, ctx.MkInt(12))))

    let rec loop acc count = 
        let result = solver.Check()
        if result = Status.SATISFIABLE
        then
            let m = solver.Model
            let assignments = [| for c in courses -> ctx.MkEq(c, m.Evaluate(c, true)) |]
            solver.Assert(ctx.MkNot(ctx.MkAnd assignments))
            let chosen = [ for c in courses do
                            let b = m.Evaluate(incl c, true).IsTrue
                            let n = Convert.ToInt32(m.Evaluate(number c, true).ToString())
                            if b then yield sprintf "%05d" n else () ]
            loop (chosen :: acc) (count + 1)
        else (acc,count)
    loop [] 0