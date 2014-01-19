module FileParser

open System
open Types

let toZECTS = function
    | 2.5 -> 1
    | 5.0 -> 2
    | 7.5 -> 3
    | 10.0 -> 4
    | _ -> failwith "invalid ECTS amount"

let fromZECTS = function
    | 1 -> 2.5
    | 2 -> 5.0
    | 3 -> 7.5
    | 4 -> 10.0
    | _ -> failwith "invalid ECTS representation"

let toCode (str : string) = 
    let code (s:string) =
        match s.Substring(1) with
        | "1A" -> (1, A)
        | "1B" -> (1, B)
        | "2A" -> (2, A)
        | "2B" -> (2, B)
        | "3A" -> (3, A)
        | "3B" -> (3, B)
        | "4A" -> (4, A)
        | "4B" -> (4, B)
        | "5A" -> (5, A)
        | "5B" -> (5, B)
        | "1" -> (1, X)
        | "2" -> (2, X)
        | "3" -> (3, X)
        | "4" -> (4, X)
        | "5" -> (5, X)
        | _ -> failwithf "unrecognized code: %s" str
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

let toZCourse (a : Course) = { ZNo = toNumber a.CourseNo; ZCode = toZCode a.Placement; ZECTS = toZECTS a.ECTS }
let fromZCourse (a : ZCourse) = { CourseNo = fromNumber a.ZNo; CourseName = ""; Placement = fromZCode a.ZCode; ECTS = fromZECTS a.ZECTS }

let toZEncoding xs = List.map toZCourse xs
let fromZEncoding xs = List.map fromZCourse xs

let readlines filename = System.IO.File.ReadAllLines(filename) |> List.ofArray

let cleanlines lines =
    lines
    |> List.tail
    |> List.filter (fun s -> not (String.IsNullOrEmpty s))

let convert (lines : string list) =
    lines
    |> List.map (fun s ->
                    s.Split([|','|])
                    |> List.ofArray
                    |> function
                    | [cno; n; p; e] ->
                        let ects = Convert.ToDouble(e)
                        { CourseNo = cno; CourseName = n; Placement = (toCode p); ECTS = ects }
                    | _ -> failwith "incorrect format")
        
let readFromFile = readlines >> cleanlines >> convert

let readConstraints filename =
    let lines =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun e -> e.Split([|':'|]).[1])
        |> Array.toList
        |> List.map (fun e -> e.Split([|','|]) |> Array.toList |> List.map ((fun s -> s.Trim()) >> toCode))
    match lines with
    | [no; maybes] -> (no,maybes)
    | _ -> failwith "incorrect constraints format"
