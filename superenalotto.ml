(*
Date le ultime X estrazioni del superenalotto (sequenze di 6
numeri compresi tra 1 e 90), determinare i 6 numeri che più probabilmente
usciranno alla prossima estrazione.
In generale: data una lista contenente liste di interi estrazioni: int list list,
dove ogni sottolista contiene DIM elementi, compresi tra 1 e HIGHER,
determinare i DIM numeri che occorrono nella lista un minor numero di volte *)

(* super: int list list -> int -> int -> int list *)

(* upto: int -> int -> int list *)
let rec upto n m =
    if n > m then []
    else 
        n::upto (n+1) m

let rec conta n = function
      [] -> 0
    | x::rest -> if x = n then  
                    1 + conta n rest
                else
                    conta n rest

let rec conta_tutti elementi listona =
    match elementi with
      [] -> []
    | x::rest -> (x, conta x listona)::conta_tutti rest listona


let rec take n = function
     [] -> []
    | x::rest -> if n > 0 then
                    x::take (n-1) rest 
                else []

(* minore tra due coppie *)
(* minore: 'a * 'b -> 'c * 'b -> bool *)
let minore (_,x) (_,y) = x < y

(* 'a list -> 'a list * 'a list *)
let rec split = function
      [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::rest -> let (xs, ys) = split rest 
                    in (x::xs, y::ys)

let rec merge xs ys = 
    match (xs,ys) with
      ([], _) -> ys
    | (_, []) -> xs
    | (x::xs, y::ys) -> if minore x y then x::merge xs (y::ys)
                        else y::merge (x::xs) ys

(* sort: 'a list -> 'a list *)
(*  sort : ('a * 'b) list -> ('a * 'b) list *)
let rec sort = function
      [] -> []
    | [x] -> [x]
    | lst -> let (xs, ys) = split lst
             in merge (sort xs) (sort ys)

let rec primi = function
      [] -> []
    | (x, _)::rest -> x::primi rest
