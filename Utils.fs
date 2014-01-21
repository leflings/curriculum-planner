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

module Encoding =
    let toZECTS (f:float) = int (f/2.5)

    let fromZECTS i = float(i)*2.5

    let toCode (str : string) = 
        match str with
        | "Januar" -> Jan
        | "Juni" -> Jun
        | _ -> let code (s:string) =
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
        | Jun -> -1000
        | Jan -> -2000

    let fromZCode i =
        match i with
        | -1000 -> Jun
        | -2000 -> Jan
        | i -> if i >= 10 then F(i/10, X) else F(i, if i > 0 then A else B)

    let fromCode c =
        let period = function | A -> "A" | B -> "B" | X -> ""
        match c with
        | F(i,p) -> sprintf "F%d%s" i (period p)
        | E(i,p) -> sprintf "E%d%s" i (period p)
        | Jan -> "Januar"
        | Jun -> "Juni"

    let toNumber (s:string) = Convert.ToInt32(s)
    let fromNumber i = sprintf "%05d" i

    let toZCourse (a : Course) = { ZNo = toNumber a.CourseNo; ZCode = toZCode a.Code; ZECTS = toZECTS a.ECTS; ZMandatory = false }
    let fromZCourse (a : ZCourse) = { CourseNo = fromNumber a.ZNo; CourseName = ""; Code = fromZCode a.ZCode; ECTS = fromZECTS a.ZECTS; Prereqs= [] }

    let toZEncoding xs = List.map toZCourse xs
    let fromZEncoding xs = List.map fromZCourse xs

    let isClash a b = a = b || 10 = abs ((max a b)/(min a b))
    let countClashes cmap maybeConstraints selection =
        List.fold
            (fun acc e ->
                let zc = (Map.find e cmap).Code |> toZCode
                if List.exists (isClash zc) maybeConstraints then acc + 1 else acc) 0 selection

module Semester =
    let getECTS = function | Semester(p,_,_,_) -> match p with | Spring(_,e) | Fall(_,e) | June(_,e) | January(_,e) -> e
    let getConstraints = function | Semester(_,c,_,_) -> c
    let getMandatory = function | Semester(_,_,_,m) -> m
    let getCourses = function | Semester(_,_,cs,_) -> cs
    let setCourses s cs = match s with | Semester(p,c,_,m) -> Semester(p,c,cs,m) 
    let getName (s : Semester) = match s with | Semester(p,_,_,_) -> match p with | Spring(n,_) | Fall(n,_) | January(n,_) | June(n,_) -> n

    let isFilled s =
        let (el, eh) = getECTS s
        let sum = match s with | Semester(_,_,s,_) -> s |> Set.toSeq |> Seq.sumBy (fun e -> e.ECTS)
        el <= sum && sum <= eh

    let printSemester (semester : Semester) = 
        let am = Array.init 5 (fun _ -> "")
        let pm = Array.init 5 (fun _ -> "")
        let setFromCode str c =
            match c with
            | Jan | Jun -> Array.fill am 0 5 str
                           Array.fill pm 0 5 str
            | F(i,p) | E(i,p) ->
                match p with
                | A -> if i % 2 <> 0
                       then am.[i/2] <- str + am.[i/2]
                       else pm.[i/3] <- str + pm.[i/3]
                | B -> match i with
                       | 1 -> pm.[3] <- str + pm.[3]
                       | 2 -> am.[3] <- str + am.[3]
                       | 3 -> am.[4] <- str + am.[4]
                       | 4 -> pm.[4] <- str + pm.[4]
                       | 5 -> pm.[2] <- str + pm.[2]
                       | _ -> failwith "Wrong code"
                | X -> match i with
                       | 1 -> am.[0] <- str + am.[0]; pm.[3] <- str + pm.[3]
                       | 2 -> pm.[0] <- str + pm.[0]; am.[3] <- str + am.[3]
                       | 3 -> am.[1] <- str + am.[1]; am.[4] <- str + am.[4]
                       | 4 -> pm.[1] <- str + pm.[1]; pm.[4] <- str + pm.[4]
                       | 5 -> am.[2] <- str + am.[2]; pm.[2] <- str + pm.[2]

                       | _ -> failwith "Wrong code"
        let (period,cs,hard,soft) = match semester with | Semester(p,(h,s),cs,_) -> (p,cs,h,s)

        cs |> Set.iter (fun e -> let name = sprintf "%s" e.CourseNo
                                 setFromCode name e.Code )
        hard |> List.iter (setFromCode "H ")
        soft |> List.iter (setFromCode "S ")

        let cw = 12
        let filler = String.replicate cw "-"
        let delimiterPattern =
            let f = String.replicate cw "-"
            (String.replicate 6 ("+"+f)) + "+"
        let linePattern = 
            let s = (String.replicate 6 ("| %"+ string(cw-2) + "s ")) + "|"
            Printf.TextWriterFormat<string -> string -> string -> string -> string -> string -> unit>(s)
        printfn "\n%s" ((getName semester).ToUpper())
        printfn "%s" delimiterPattern
        printfn linePattern "" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
        printfn "%s" delimiterPattern
        printfn linePattern "8-12" am.[0] am.[1] am.[2] am.[3] am.[4]
//        printfn "%s" delimiterPattern
//        printfn linePattern "12-13" "" "" "" "" ""
        printfn "%s" delimiterPattern
        printfn linePattern "13-17" pm.[0] pm.[1] pm.[2] pm.[3] pm.[4]
        printfn "%s" delimiterPattern