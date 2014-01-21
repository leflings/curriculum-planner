#r "Microsoft.Z3.dll"
#load "Types.fs"
#load "Utils.fs"
#load "FileParser.fs"
#load "Scheduler.fs"
#load "CurriculumPlanner.fs"

open Types
open Utils
open FileParser
open Scheduler
open CurriculumPlanner

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#time "on";;

let _ = 
    let courselist = readFromFileWithPrereqs "big-with-prereqs.csv"

    let fall1 =
        Semester(
            Fall( // Type of semester (Fall,Spring,June,January)
                "Fall '14", // Name of semester
                (25.0,30.0) // min max amount of ECTS that should be obtained
            ),
            (
                [F(1,A);F(2,A)], // Hard constraints
                [F(3,B);F(4,B)] // Soft constraints
            ),
            Set.empty, // The set of planned courses (updated after scheduling)
            ["02342"] // List of numbers of mandatory courses / courses student wants to take
        )
    let jan1 = Semester(January("January '15", (5.0,5.0)), ([],[]), Set.empty, [])
    let spring1 = Semester(Spring("Spring '15", (25.0,30.0)), ([],[]), Set.empty, ["02141"])
    let june1 = Semester(June("June '15", (5.0,5.0)), ([],[]), Set.empty, [])
    let fall2 = Semester(Fall("Fall '15", (25.0,30.0)), ([],[F(1,A);F(2,A);F(3,A);F(4,A);F(5,A);F(5,B)]), Set.empty, [])
    let jan2 = Semester(January("January '16", (5.0,5.0)), ([],[]), Set.empty, [])

    let semesters = planSemesters courselist [fall1; jan1; spring1; june1; fall2; jan2]
    List.iter Semester.printSemester semesters
#time "off";;