module Main

open Types
open FileParser
open Utils
open Scheduler
open CurriculumPlanner
open Nqueens


[<EntryPoint>]
let main argv =
    let courselist = readFromFileWithPrereqs "big-with-prereqs.csv"

    let fall1 = Semester(Fall("Fall '14", (25.0,30.0)), ([F(1,A);F(2,A)],[F(3,B);F(4,B)]), Set.empty, [])
    let jan1 = Semester(January("January '15", (5.0,5.0)), ([],[]), Set.empty, [])
    let spring1 = Semester(Spring("Spring '15", (25.0,30.0)), ([],[]), Set.empty, ["02141"])
    let june1 = Semester(June("June '15", (5.0,5.0)), ([],[]), Set.empty, [])
    let fall2 = Semester(Fall("Fall '15", (25.0,30.0)), ([],[F(1,A);F(2,A);F(3,A);F(4,A);F(5,A);F(5,B)]), Set.empty, [])
    let jan2 = Semester(January("January '16", (5.0,5.0)), ([],[]), Set.empty, [])

    let semesters = planSemesters courselist [fall1; jan1; spring1; june1; fall2; jan2]
    List.iter Semester.printSemester semesters
    System.Console.ReadLine() |> ignore
    0
    