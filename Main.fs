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
            let zc = (Map.find e cmap).Code |> Encoding.toZCode
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
    let fall1 = Semester(Fall("Fall 14", (25.0,30.0)), ([F(1,A);F(2,A)],[F(3,B);F(4,B)]), Set.empty)
//    let fall1 = Semester(Spring("Fall 14", (55.0,60.0)), ([],[]), Set.empty)
    let jan1 = Semester(January("January 15", (5.0,5.0)), ([],[]), Set.empty)
    let spring1 = Semester(Spring("Spring 15", (25.0,30.0)), ([],[]), Set.empty)
    let june1 = Semester(June("June 15", (5.0,5.0)), ([],[]), Set.empty)
    let fall2 = Semester(Fall("Fall 15", (25.0,30.0)), ([F(1,A)],[F(2,A);F(3,A);F(4,A);F(5,A);F(5,B)]), Set.empty)
    let jan2 = Semester(January("January 16", (5.0,5.0)), ([],[]), Set.empty)

    let semesters = planSemesters [fall1; jan1; spring1; june1; fall2; jan2]
    List.iter Semester.printSemester semesters
    System.Console.ReadLine() |> ignore
    0
    