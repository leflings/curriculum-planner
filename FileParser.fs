module FileParser

open System
open Types
open Utils
open Utils.Encoding

module Helpers =
    let readlines filename = System.IO.File.ReadAllLines(filename) |> List.ofArray

    let cleanlines lines =
        lines
        |> List.tail
        |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))

    let convert (lines : string list) =
        lines
        |> List.map (fun s ->
                        s.Split([|','|])
                        |> Array.map (fun s -> s.Trim())
                        |> function
                        | [|cno; n; p; e|] ->
                            let ects = Convert.ToDouble(e)
                            {
                              CourseNo = cno;
                              CourseName = n;
                              Code = (toCode p);
                              ECTS = ects;
                              Prereqs = []
                            }
                        | _ -> failwith "incorrect format")

    let convertWithPrereqs (lines : string list) =
        lines
        |> List.map (fun s ->
                        s.Split([|','|])
                        |> Array.map (fun s -> s.Trim())
                        |> function
                        | [|cno; n; p; e; pre|] ->
                            let ects = Convert.ToDouble(e)
                            {
                              CourseNo = cno;
                              CourseName = n;
                              Code = (toCode p);
                              ECTS = ects;
                              Prereqs = pre.Split([|';'|]) |> List.ofArray |> List.filter (not << String.IsNullOrWhiteSpace)
                            }
                        | _ -> failwith "incorrect format")
        
open Helpers
let coursesFromFile = readlines >> cleanlines >> convert
let coursesFromFileWithPrereqs = readlines >> cleanlines >> convertWithPrereqs

let constraintsFromFile filename =
    let lines =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun e -> e.Split([|':'|]).[1])
        |> Array.toList
        |> List.map (fun e -> e.Split([|','|]) |> Array.toList |> List.map ((fun s -> s.Trim()) >> toCode))
    match lines with
    | [hard; soft] -> (hard,soft)
    | _ -> failwith "incorrect constraints format"
