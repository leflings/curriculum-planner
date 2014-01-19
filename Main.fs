module Main

open Types
open FileParser
open Utils
open Scheduler
open Nqueens

let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }


[<EntryPoint>]
let main argv =
    let courses = readFromFile "big.csv"
    let zcourses = List.map toZCourse courses
    let cmap =
        courses
        |> List.map (fun e -> (e.CourseNo, e))
        |> Map.ofList

    let (no,maybe) = readConstraints "constraints.txt"
    let constraints = List.map toZCode no
    let maybeConstraints = List.map toZCode maybe

    let maybeCombos = allCombinations maybeConstraints

    let cons = List.map (List.append constraints) maybeCombos

    let rec findFirst = function
        | [] -> []
        | x::xs ->
            let s = schedule zcourses x
            if s <> [] then s else findFirst xs

    let choice = findFirst cons

    let selectedCourses = List.map (fun e -> Map.find e cmap) choice
    List.iter (printfn "%A") selectedCourses

//    let (solutions, count) = Scheduler.countSolutions zcourses constraints
//    printfn "Solutions: %d" count
//    let sortedSolutions =
//        List.map (fun e -> ((countClashes e), e)) solutions
//        |> List.sortBy fst
//    List.iter (fun s -> printfn "%A" s) sortedSolutions
    0
    