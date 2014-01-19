module Program

open Nqueens

let N = 10

let _ = solve N
let count = countSolutions N
printfn "Solutions: %d" count
printfn "Press any key to finish..."
System.Console.ReadKey() |> ignore