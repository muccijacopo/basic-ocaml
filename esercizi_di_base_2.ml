(* length: 'a list -> int *)
(* Versione ricorsiva *)
let rec length = function
    [] -> 0
  | _::rest -> 1 + length rest

(* Versione tail-recursive *)
(* aux: int -> 'a list -> int *)
let length_it list =
    let rec aux n = function
        [] -> n
      | _::rest -> aux (n+1) rest
    in aux 0 list

(* sumof: int list -> int *)
let rec sumof = function
    [] -> 0
  | x::rest -> x + sumof rest

(* Versione tail-recursive *)
(* aux: int -> int list -> int *)
let sumof_it lst =
  let rec aux sum = function
      [] -> sum
    | x::rest -> aux (sum+x) rest
  in aux 0 lst

(* maxlist: 'a list -> 'a *)
let maxlist_it list =  
    match list with
        [] -> failwith "Lista vuota"
      | x::rest ->
                    let rec aux x = function
                        [] -> x
                      | y::rest -> aux (max x y) rest

                    in aux 0 list

(* Versione ricorsiva *)
let rec maxlist = function 
    [] -> failwith "Lista vuota"
  | [x] -> x 
  | x::rest -> max x (maxlist rest)

let rec take n = function 
    [] -> []
  | x::rest -> if n<=0 then []
               else 
                  x::take (n-1) rest 


(* drop: int -> 'a list -> 'a list *)
let rec drop n = function
    [] -> []
  | x::rest as lista -> if n > 0 then
                    drop (n-1) rest
                else
                    lista

(* append: a' list -> 'a list -> 'a list *)
let rec append list1 list2 =
    match list1 with
        [] -> list2
      | x::rest -> x::append rest list2

(* reverse: 'a list -> 'a list *)
let rec reverse = function
      [] -> []
    | x::rest -> reverse rest @ [x]

(* Versione tail recursive *)
let reverse_it list =
    let rec aux newList = function
          [] -> newList
        | x::rest -> aux (x::newList) rest
    in aux [] list

(* nth: int -> 'a list -> 'a *)
let rec nth n = function
    [] -> failwith "Lista vuota o troppo corta" 
    | x::rest -> 
            if n > 0 then nth (n-1) rest
            else if n = 0 then x 
            else failwith "n negativo"


(* remove: 'a -> 'a list -> 'a list *)   
let rec remove n = function
      [] -> []
    | x::rest ->
                if n = x then 
                    remove n rest
                else 
                    x::remove n rest

(* enumera 'a list -> (int * 'a) list *)
let rec enumera list =
    let rec aux c = function
          [] -> []
        | x::rest -> (c, x)::aux (c+1) rest
    in aux 0 list

(* position: 'a -> 'a list -> int *)
let position x list =
    let rec aux pos = function
      [] -> failwith "non presente"
    | y::rest -> 
                    if x == y then pos
                    else
                        aux (pos+1) rest
    in aux 0 list 

(* alternate: mostra solamente gli elementi in posizione dispari *)
(* alternate: 'a list -> 'a list *)
let rec alternate = function
      _::y::rest -> y::alternate rest
    | _ -> []

(* min_dei_max: data una lista di liste id interi, riporta il valore minimo tra i massimi di ciasciuna lista *)
let rec min_dei_max = function
      [] -> failwith "errore" 
    | [lst] -> maxlist lst
    | lst::rest -> min (maxlist lst ) (min_dei_max rest)

let compare x y = 
  if x > y then 1
  else if x = y then 0
  else -1
  
