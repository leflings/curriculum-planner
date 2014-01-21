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

let getSemesterName (s : Semester) = match s with | Semester(p,_,_) -> match p with | Spring(n,_) | Fall(n,_) | January(n,_) | June(n,_) -> n

let printSemester (semester : Semester) = 
    let am = Array.init 5 (fun _ -> "")
    let pm = Array.init 5 (fun _ -> "")
    let setFromCode name c =
        match c with
        | F(i,p) | E(i,p) ->
            match p with
            | A -> if i % 2 <> 0
                   then am.[i/2] <- name
                   else pm.[i/3] <- name
            | B -> match i with
                   | 1 -> am.[3] <- name
                   | 2 -> pm.[3] <- name
                   | 3 -> am.[4] <- name
                   | 4 -> pm.[4] <- name
                   | 5 -> pm.[2] <- name
                   | _ -> failwith "Wrong code"
            | X -> match i with
                   | 1 -> am.[0] <- name; pm.[3] <- name
                   | 2 -> pm.[0] <- name; am.[3] <- name
                   | 3 -> am.[1] <- name; am.[4] <- name
                   | 4 -> pm.[1] <- name; pm.[4] <- name
                   | 5 -> am.[2] <- name; pm.[2] <- name
                   | _ -> failwith "Wrong code"
    let (cs,hard,soft) = match semester with | Semester(_,(h,s),cs) -> (cs,h,s)
    cs |> Set.iter (fun e -> let name = sprintf "%s" e.CourseNo
                             setFromCode name e.Code )
    hard |> List.iter (setFromCode "XXX")
    soft |> List.iter (setFromCode "+++")
    let cw = 11
    let filler = String.replicate cw "-"
    let format s = Printf.TextWriterFormat<string -> string -> string -> string -> string -> string -> unit>(s)
    let delimiterPattern =
        let f = String.replicate cw "-"
        (String.replicate 6 ("+"+f)) + "+"
    let linePattern = (String.replicate 6 ("| %"+ string(cw-2) + "s ")) + "|"
    printfn "\n%s" ((getSemesterName semester).ToUpper())
    printfn "%s" delimiterPattern
    printfn (format linePattern) "" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
    printfn "%s" delimiterPattern
    printfn (format linePattern) "8-12" am.[0] am.[1] am.[2] am.[3] am.[4]
    printfn "%s" delimiterPattern
    printfn (format linePattern) "12-13" "" "" "" "" ""
    printfn "%s" delimiterPattern
    printfn (format linePattern) "13-17" pm.[0] pm.[1] pm.[2] pm.[3] pm.[4]
    printfn "%s" delimiterPattern