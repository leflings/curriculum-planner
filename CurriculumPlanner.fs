module CurriculumPlanner

open Types
open Utils
open FileParser
open Scheduler

let semesterECTS = function | Semester(p,_,_) -> match p with | Spring(_,e) | Fall(_,e) | June(_,e) | January(_,e) -> e
let semesterConstraits = function | Semester(_,c,_) -> c

let setSemesterCourses s cs = match s with | Semester(p,c,_) -> Semester(p,c,cs) 

let isSemesterFilled s =
    let (el, eh) = semesterECTS s
    let sum = match s with | Semester(_,_,s) -> s |> Set.toSeq |> Seq.sumBy (fun e -> e.ECTS)
    el <= sum && sum <= eh

let consCombos (h,s) =
    let hard = List.map toZCode h
    let soft = List.map toZCode s |> allCombinations
    List.map (List.append hard) soft

let rec findFirst (ls : seq<string list> list) =
    match ls with
    | [] -> Seq.empty
    | x::xs ->
        let h = Seq.nth 0 x
        if h <> [] then x else findFirst xs

let planMasterStudy =
    let fall1 = Semester(Spring("Fall 14", (25.0,30.0)), ([F(1,A);F(2,A)],[F(3,B);F(4,B)]), Set.empty)
    let jan1 = Semester(January("January 15", (5.0,5.0)), ([],[]), Set.empty)
    let spring1 = Semester(Spring("Spring 15", (25.0,30.0)), ([],[]), Set.empty)
    let june1 = Semester(June("June 15", (5.0,5.0)), ([],[]), Set.empty)
    let fall2 = Semester(Fall("Fall 15", (25.0,30.0)), ([],[]), Set.empty)
    let jan2 = Semester(January("January 16", (5.0,5.0)), ([],[]), Set.empty)

    let semesters = [fall1; jan1; spring1; june1; fall2; jan2]

    let courselist = readFromFile "big.csv"
    let coursemap = courselist |> List.map (fun e -> (e.CourseNo, e)) |> Map.ofList
    let courseset = courselist |> Set.ofList
    let (woPrereqs, wPrereqs) = courseset |> Set.partition (fun e -> e.Prereqs = [])

    let planSemester (acc,(completed, eligible, ineligible)) semester =
        let (eligible', ineligible') =
            let (a,b) = ineligible |> Set.partition (fun e -> let pset = Set.ofList e.Prereqs
                                                              Set.isSubset pset completed)
            (Set.union a eligible, b)

        let scheduler = schedule (toZEncoding (Set.toList eligible')) (semesterECTS semester)

        // Makes a scheduler for all combinations of constraints. First in the list, is the one
        // With all constraints (soft and hard) fulfilled
        let options = List.map scheduler (consCombos (semesterConstraits semester))

        // The string list of course numbers of the first constraint combination that yielded a result
        let chosenNumbers = findFirst options |> Seq.head

        // The Courses of prior list
        let choice = chosenNumbers |> List.map (fun e -> Map.find e coursemap) |> Set.ofList 

        let eligible'' = Set.difference eligible' choice
        let completed' = Set.union completed (Set.ofList chosenNumbers)
        let acc' = (setSemesterCourses semester choice) :: acc
        (acc', (completed', eligible'', ineligible'))

    let semesters' = List.fold planSemester ([], (Set.empty, woPrereqs, wPrereqs)) semesters

    fst semesters'