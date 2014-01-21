module Utils

open System
open Types

let rec choices = function
  | []      -> []
  | x::xs -> (x,xs) :: [ for (a,b) in choices xs -> (a,b) ];;

let rec combinations xs i =
    [ if i=0 then yield [] else
            for (e,r) in choices xs do
                for o in combinations r (i-1) do yield e::o  ];;

let allCombinations xs = [] :: [ for i in 1..(List.length xs) do yield! combinations xs i ] |> List.rev

let toZECTS (f:float) = int (f/2.5)

let fromZECTS i = float(i)*2.5

let toCode (str : string) = 
    let code (s:string) =
        let i = s.Substring(1,1) |> Convert.ToInt32
        let p = s.Substring(2) |> function | "A" -> A | "B" -> B | _ -> X
        (i,p)
    match str.[0] with
    | 'F' -> F (code str)
    | 'E' -> E (code str)
    | _ -> failwithf "unrecognized code: %s" str


let toZCode (c:Code) =
    match c with
    | E(i, p) | F(i, p) ->
        match p with
        | A -> i
        | B -> -i
        | X -> i*10

let fromZCode i =
    if i >= 10
    then
        F(i/10, X)
    else
        F(i, if i > 0 then A else B)

let fromCode c =
    let period = function | A -> "A" | B -> "B" | X -> ""
    match c with
    | F(i,p) -> sprintf "F%d%s" i (period p)
    | E(i,p) -> sprintf "E%d%s" i (period p)

let toNumber (s:string) = Convert.ToInt32(s)
let fromNumber i = sprintf "%05d" i

let toZCourse (a : Course) = { ZNo = toNumber a.CourseNo; ZCode = toZCode a.Code; ZECTS = toZECTS a.ECTS }
let fromZCourse (a : ZCourse) = { CourseNo = fromNumber a.ZNo; CourseName = ""; Code = fromZCode a.ZCode; ECTS = fromZECTS a.ZECTS; Prereqs= [] }

let toZEncoding xs = List.map toZCourse xs
let fromZEncoding xs = List.map fromZCourse xs