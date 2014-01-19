#r "Microsoft.Z3.dll"
#load "Nqueens.fs"

open Nqueens

let N = 10

#time "on";;

let result = solve N;;
let count = countSolutions N;;
