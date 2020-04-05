exception NotFound

(* assoc: 'a -> ('a * 'b) list -> 'b *)
let rec assoc k = function
[] -> raise NotFound
| (k1,v)::rest -> if k = k1 then v
else assoc k rest

(* cancella: 'a -> ('a * 'b) list -> ('a * 'b) list *)
let rec cancella k = function
      [] -> []
    | (x, y)::rest -> if k = x then cancella k rest
                      else (x,y)::cancella k rest


(* mem: 'a -> 'a list -> bool *)
let rec mem e = function
      [] -> false
    | x::rest -> e = x || mem e rest

let rec sumof = function
      [] -> 0
    | x::rest -> x + sumof rest

(* BACKTRACKING *)
let subset_search set n =
    let rec aux solution altri =
        let somma = sumof solution in
        if somma > n then raise NotFound
        else if somma = n then solution
        else match altri with
          [] -> raise NotFound
        | x::rest ->
                        try aux (x::solution) rest
                        with NotFound -> aux solution rest
    in aux [] set 


let rec split = function
    [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::rest -> 
                  let (prima, seconda) = split rest 
                  in (x::prima, y::seconda)


let listlist = [[1];[2];[3]];;

let cons x lst = x::lst

(* Lista di tutti i sottinsiemi di S *)
let rec powerset = function 
    [] -> [[]]
  | x::rest -> 
              let powerset_rest = powerset rest
              in powerset_rest @ List.map (cons x) powerset_rest


let rec cartprod set1 set2 =
match set1 with
    [] -> []
  | x::rest ->
            (List.map (function y -> (x,y)) set2) @ cartprod rest set2



(* ------------------------------ *)
(* ESERCIZI proposti *)

(* filter_vicini_it: int -> (int * int) list -> (int * int) list *)
(* aux: int -> (int * int) list -> (int * int) list -> (int * int) list *)
(* let filter_vicini dim lst =
  let rec aux result = function
      [] -> []
    | casella::rest -> if in_labirinto dim casella then 
                          aux (casella::result) rest 
                      else 
                          aux result rest
  in aux [] lst
*)

(*
let raccolti contenuti caselle =
  let rec aux result = function
      [] -> []
    | casella::rest -> aux ((find_content contenuti casella) @ result) rest
  in [] lst 
*) 

let rec combine l1 l2 =
    if List.length l1 <> List.length l2 then
        failwith "Liste di lunghezza diverse"
    else
      match (l1, l2) with 
        ([], []) -> []
      | (x::restx, y::resty) -> (x, y)::combine restx resty

(* invece di fare una if potevo utilizzare il pattern "_" *)

let rec split = function
    [] -> ([], [])
  | (x, y)::rest -> let (restx, resty) = split rest
                    in (x::restx, y::resty)


let rec cancella a = function
    [] -> []
  | (x,y)::rest -> if a == x then 
                      cancella a rest
                    else
                      (x,y)::cancella a rest

(* si può fare anche con il funzionale filter *)
let cancella k assoclist =
  List.filter (function (x,_) -> x<>k) assoclist


(* Se si rappresentano insiemi finiti mediante liste senza ripetizioni, implementare
le operazioni di unione, intersezione, differenza *)
let rec union i1 i2 =
    match i2 with
      [] -> i1 
    | x::rest -> if List.mem x i1 then union i1 rest
                else x::union i1 rest 

let rec intersect i1 i2 =
      match i2 with
      [] -> []
    | x::rest -> if List.mem x i1 then x::intersect i1 rest
                 else intersect i1 rest

let subset s1 s2 = List.for_all (function x -> List.mem x s2) s1

let insert x = List.map (function y -> (x, y) )

let rec upto n = 
  if n<= 0 then []
  else upto (n-1)@[n]



let intpairs x =
  let lst = upto x in
    let rec aux = function
      [] -> []
    | x::rest -> (insert x lst) @ aux rest
    in aux lst

(* durations_to_end_times: int list -> int list *)
(* durations_to_end_times lista = lista in cui ogni elemento è la somma dei precedenti *)
let rec durations_to_end_times lista = 
  let rec aux time = function
    [] -> []
  | x::rest -> (x+time)::aux(x+time) rest
  in aux 0 lista


(* ESERCIZIO 1 ESAME SETTEMBRE 2018 *)
(* is_sorted: 'a list -> bool *)
(* is_sorted lista = true se la lista è ordinata in modo crescente *)
let rec is_sorted = function
  [] | [_] -> true
  | x::y::rest -> x <= y && is_sorted (y::rest)


(* ESAME LUGLIO 2017 - ESERCIZIO 1 *)
(* some_all: ('a -> bool) -> 'a list list -> 'a list *)
(* some_all predicato listona = lista di listona che rispetta il predicato *)
let rec some_all p = function
    [] -> raise NotFound
  | list::rest -> if List.for_all p list then list else some_all p rest

(* ESAME SETTEMBRE 2017 *)
(* permut: 'a list -> 'a list -> bool *)
(* permut lista1 lista2 = true se lista1 è una permutazione di lista2 *)
let rec permut lista1 lista2 =
  if List.length lista1 <> List.length lista2 then false
  else
  let rec aux = function 
      [] -> true
    | x::rest -> List.mem x lista1 && aux rest
  in aux lista2


