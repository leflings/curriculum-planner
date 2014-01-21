module Main

open Types
open FileParser
open Utils
open Scheduler
open CurriculumPlanner
open Nqueens

let isClash a b = a = b || 10 = abs ((max a b)/(min a b))

let countClashes cmap maybeConstraints selection =
    List.fold
        (fun acc e ->
            let zc = (Map.find e cmap).Code |> toZCode
            if List.exists (isClash zc) maybeConstraints then acc + 1 else acc) 0 selection


[<EntryPoint>]
let main argv =
//    let courses = readFromFile "medium.csv"
//    let zcourses = List.map toZCourse courses
//    let cmap = courses |> List.map (fun e -> (e.CourseNo, e)) |> Map.ofList
//
//    let (hard,soft) = let (h,s) = readConstraints "constraints.txt"
//                      (List.map toZCode h, List.map toZCode s)
//
//    let softCombinations = allCombinations soft
//
//    let constraintCombinations = List.map (List.append hard) softCombinations
//
//    let choices = List.map (schedule zcourses (25.0,30.0)) constraintCombinations
//
//    let rec findFirst (ls : seq<string list> list) =
//        match ls with
//        | [] -> Seq.empty
//        | x::xs ->
//            let h = Seq.nth 0 x
//            if h <> [] then x else findFirst xs
//
//    let firstChoice = findFirst choices
//    let first = Seq.head firstChoice
//
//    let selectedCourses = List.map (fun e -> Map.find e cmap) first
//    List.iter (printfn "%A") selectedCourses
    let semesters = planMasterStudy
    List.iter printSemester semesters
//    List.iter (fun (Semester(s,c,cs)) -> printfn "\n%A" s; Set.iter (printfn "%A") cs) semesters
    System.Console.ReadLine() |> ignore
    0
    