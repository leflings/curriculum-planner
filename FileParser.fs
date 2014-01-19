module FileParser

open System
open Types
open Utils

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
