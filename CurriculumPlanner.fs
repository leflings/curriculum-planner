module CurriculumPlanner

open Types
open Utils
open FileParser
open Scheduler

module Helpers = 
    /// Generates a list of constraint combinations starting with all hard and soft constraints
    /// then the all hard constraints and a subset of soft constraints. Last one is just the
    /// hard constraints
    let consCombos (h,s) =
        let hard = List.map Encoding.toZCode h
        let soft = List.map Encoding.toZCode s |> allCombinations
        List.map ((@) hard) soft

    let rec findFirst (ls : seq<string list option> list) =
        match ls with
        | [] -> None
        | x::xs -> match Seq.head x with
                   | Some _ as t -> t
                   | None -> findFirst xs

    let filterCourses s cs =
        let f = match s with
                | Semester (p,_,_,_) ->
                    match p with
                    | Spring _ -> (fun (e:Course) -> match e.Code with | F _ -> true | _ -> false)
                    | Fall _  -> (fun (e:Course) -> match e.Code with | E _ -> true | _ -> false)
                    | January _ -> (fun (e:Course) -> match e.Code with | Jan _ -> true | _ -> false)
                    | June _ -> (fun (e:Course) -> match e.Code with | Jun _ -> true | _ -> false)
        List.filter f cs

    let checkMandatory mset ls =
        if Set.isEmpty mset then ls else
        List.map (fun e -> 
                    if Set.contains (Encoding.fromNumber e.ZNo) mset
                    then
                        { ZNo = e.ZNo;
                          ZECTS = e.ZECTS;
                          ZCode = e.ZCode;
                          ZMandatory = true }
                    else e ) ls

open Helpers
let planSemesters courselist semesters =
    let coursemap = courselist |> List.map (fun e -> (e.CourseNo, e)) |> Map.ofList
    let courseset = courselist |> Set.ofList
    let (woPrereqs, wPrereqs) = courseset |> Set.partition (fun e -> e.Prereqs = [])

    let planSemester (acc,(completed, eligible, ineligible)) semester =
        // Move courses from ineligible to eligible based on the list of completed courses
        let (eligible', ineligible') =
            let (a,b) = ineligible |> Set.partition (fun e -> let pset = Set.ofList e.Prereqs
                                                              Set.isSubset pset completed)
            (Set.union a eligible, b)

        // Bake in arguments for the scheduler
        let scheduler = let mset = Semester.getMandatory semester |> Set.ofList
                        let cs = Set.toList eligible'
                                 |> filterCourses semester 
                                 |> Encoding.toZEncoding
                                 |> checkMandatory mset
                        let ectsMinMax = (Semester.getECTS semester)
                        schedule cs ectsMinMax

        // Makes a scheduler for all combinations of constraints. First in the list, is the one
        // With all constraints (soft and hard) fulfilled
        let options = let constraints = (consCombos (Semester.getConstraints semester))
                      List.map scheduler constraints

        // The string list of course numbers of the first constraint combination that yielded a result
        let chosenNumbers = findFirst options |> function | Some x -> x | None -> []

        // The Courses of prior list
        let courses = chosenNumbers |> List.map (fun e -> Map.find e coursemap) |> Set.ofList 

        // Remove chosen courses from eligible pile
        let eligible'' = Set.difference eligible' courses
        // Add chosen course numbers to list of completed courses
        let completed' = Set.union completed (Set.ofList chosenNumbers)
        // Add the updated semester to the accumulator
        let acc' = (Semester.setCourses semester courses) :: acc

        (acc', (completed', eligible'', ineligible'))

    let semesters' = let acc = ([], (Set.empty, woPrereqs, wPrereqs))
                     List.fold planSemester acc semesters

    fst semesters' |> List.rev