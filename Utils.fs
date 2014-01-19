module Utils

open Types
open FileParser

let isClash a b = a = b || 10 = abs ((max a b)/(min a b))

let countClashes cmap maybeConstraints selection =
    List.fold
        (fun acc e ->
            let zc = (Map.find e cmap).Placement |> toZCode
            if List.exists (isClash zc) maybeConstraints then acc + 1 else acc) 0 selection

let rec choices = function
  | []      -> []
  | p::tail -> (p,tail) :: [ for (y,l) in choices tail -> (y,l) ];;

let rec combinations S k =
    [ if k=0 then yield [] else
            for (e,r) in choices S do
                for o in combinations r (k-1) do yield e::o  ];;

let allCombinations S = [] :: [ for i in 1..(List.length S) do yield! combinations S i ] |> List.rev
