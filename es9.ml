(* DEFINIZIONE DI ALBERI N-ARI *)
(* forma generale Tr(radice, tlist) *)
type 'a ntree = Tr of 'a * 'a ntree list 
(* Costruisce alberi costituti da un unico nodo *)
let leaf x = Tr(x, [])

let ntree1 = 
Tr(1,[Tr(2,[Tr(3,[leaf 4;
leaf 5]);
Tr(6,[leaf 7]);
leaf 8]);
leaf 9;
Tr(10,[Tr(11,[leaf 12;
leaf 13;
leaf 14]);
leaf 15;
Tr(16,[leaf 17;
Tr(18,[leaf 19;
leaf 20])])])])



(* Calcolo del numero dei nodi *)
let rec sumof = function 
    [] -> 0
  | x::rest -> x + sumof rest
(* con funzioni di ordine superiore *)
let rec size1 (Tr(_, tlist)) =
  1 + sumof (List.map size1 tlist)

(* con la mutua ricorsione *)
let rec size2 (Tr(_, tlist)) =
  1 + sumofsizes tlist
and sumofsizes = function
    [] -> 0
  | t::rest -> (size2 t) + sumofsizes rest

(* preorder *)
let rec preorder (Tr(x, tlist)) = 
  x::preorder_tlist tlist
and preorder_tlist = function 
    [] -> []
  | t::rest -> (preorder t) @ (preorder_tlist rest)
(* preorder con funzioni di ordine superiore *)
let rec preorder1 (Tr(x, tlist)) =
  x::List.flatten((List.map preorder1 tlist))

let rec maxl = function
    [x] -> x
  | x::rest -> max x (maxl rest)
  | _ -> failwith "maxl"

(* height: 'a ntree -> int *)
(* con le fz di ordine superiore *)
let rec height (Tr(_, tlist)) = match tlist with
    [] -> 0
  | _ -> 1 + maxl (List.map height tlist)

(* con la mutua ricorsione *)
let rec height2 (Tr(_,tlist)) = match tlist with
    [] -> 0
  | _ -> 1 + maxheight tlist
and maxheight = function
    [] -> failwith "maxheight"
  | [t] -> height2 t
  | t::rest -> max (height2 t) (maxheight rest)

(* fattore di ramificazione = massimo numero di sottoalberi di un nodo in t *)
let rec branching_factor (Tr(_, tlist)) =
  match tlist with 
    [] -> 0
  | _ -> max (List.length tlist) (maxl (List.map branching_factor tlist))

(* occurs_in: 'a ntree -> 'a -> bool *)
(* occurs_in ntree nodo = true se nodo occore in almeno uno degli alberi in tlist *)
let rec occurs_in (Tr(x, tlist)) nodo =
  x = nodo || occurs_in_tlist nodo tlist 
and occurs_in_tlist nodo = function 
    [] -> false
  | t::rest -> occurs_in t nodo || occurs_in_tlist nodo rest

(* path: 'a ntree -> 'a -> 'a list *)
(* path: ntree y = cammino dalla radice fino a una foglia etichettata da y *)
exception NotFound
let rec path (Tr(x, tlist)) y =
  match tlist with 
    [] -> if x = y then [y]
        else raise NotFound
  | _ -> x::path_tlist y tlist
and path_tlist y = function 
  [] -> raise NotFound
| t::rest ->
    try path t y
    with NotFound -> path_tlist y rest

(* delete: 'a ntree -> 'a -> 'a ntree *)
let rec delete (Tr(x, tlist)) y =
  Tr(x, delete_tlist y tlist)
and delete_tlist y = function 
    [] -> raise NotFound
  | (Tr(x, tlist))::rest -> 
        if x = y then tlist@rest
        else try delete (Tr(x, tlist)) y::tlist
        with NotFound -> (Tr(x,rest)::(delete_tlist y tlist))


(* ESERCIZI MODULO 9 *)
(* visita in preordine *)
let rec preorder (Tr(x, tlist)) =
  x::preorder_tlist tlist
and preorder_tlist = function 
    [] -> []
  | t::rest -> preorder t @ preorder_tlist rest

(* visita in postordine *)
let rec postorder = function
  Tr(x, tlist) -> postorder_tlist x tlist
and postorder_tlist x = function 
    [] -> [x]
  | t::rest -> postorder t @ postorder_tlist x rest

let rec foglie_in_lista lst = function 
    Tr(x, []) -> List.mem x lst
  | Tr(_, tlist) -> foglie_in_lista_tlist lst tlist
and foglie_in_lista_tlist lst = function 
    [] -> true
  | t::rest -> foglie_in_lista lst t && foglie_in_lista_tlist lst rest

(* numero_foglie: 'a ntree -> int *)
let rec numero_foglie = function
    Tr(_, []) -> 1
  | Tr(_, tlist) -> numero_foglie_tlist tlist
and numero_foglie_tlist = function 
    [] -> 0
  | t::rest -> numero_foglie t + numero_foglie_tlist rest

let rec sumof = function 
    [] -> 0
  | x::rest -> x + sumof rest

let rec numero_foglie2 = function 
    Tr(x, []) -> 1
  | Tr(_, tlist) -> sumof (List.map numero_foglie2 tlist)

(* ESERCIZIO 6 *)
(* foglia_costo: 'a ntree -> (int * int) *)

(* funzione ausiliaria *)
let rec maxpair = function 
    [] -> failwith "maxpair"
  | [(x,c)] -> (x,c)
  | ((x1,c1)::(x2, c2)::rest) -> 
          if c1 > c2 then maxpair ((x1,c1)::rest)
          else maxpair ((x2,c2)::rest)

let rec foglia_costo = function 
    Tr(x, []) -> (x, x)
  | Tr(x, tlist) -> 
      let (y, c) = maxpair (List.map foglia_costo tlist)
      in (y, x+c)

  (* ESERCIZIO 7 *)
let rec tutte_foglie_costi = function 
    Tr(x, []) -> [(x,x)]
  | Tr(x, tlist) -> List.map(function (y,c) -> (y, x+c)) (List.flatten (List.map tutte_foglie_costi tlist))

  (* ESERCIZIO 8 *)
let rec ramo_da_list (Tr(x, tlist)) lista nodo = 
  match tlist with 
    [] -> 
      if x = nodo && lista = [x] then [nodo]
      else raise NotFound
  | _ -> x::ramo_da_list_bis (List.filter ((<>)x) lista) nodo tlist
and ramo_da_list_bis lista nodo = function 
    [] -> raise NotFound
  | t::rest -> 
      try ramo_da_list t lista nodo
      with NotFound -> ramo_da_list_bis lista nodo rest

  (* ESERCIZIO 9 *)

(* funzione ausiliaria *)
let is_primo n =  
    let rec aux = function
      1 -> true 
    | m -> (n mod m) <> 0 && aux (m-1)
    in (n=1 || aux (n-1))

(* ramo_di_primi: 'a ntree -> int *)
let rec ramo_di_primi = function 
    Tr(x, []) -> if is_primo x then x else raise NotFound
  | Tr(x, tlist) -> if is_primo x then aux tlist else raise NotFound
and aux = function 
    [] -> raise NotFound
  | t::rest -> 
      try ramo_di_primi t 
      with NotFound -> aux rest

  (* ESERCIZIO 10 *)
(* funzione ausiliaria *)
let p x = x mod 2 <> 0
(* path_non_pred: 'a ntree -> ('a -> bool) -> 'a list *)
let rec path_non_pred p = function 
    Tr(x, []) -> if p x then raise NotFound else [x]
  | Tr(x, tlist) -> if p x then raise NotFound else x::auxtlist p tlist
and auxtlist p = function 
    [] -> raise NotFound
  | t::rest -> 
      try path_non_pred p t
      with NotFound -> auxtlist p rest

  (* ESERCIZIO 1 ESAME LUGLIO 2018  *)
(* depth: 'a -> 'a ntree -> int *)
let rec depth x = function
    Tr(y, []) -> if x = y then 0 else raise NotFound
  | Tr(y, tlist) -> if x = y then 0 else 1 + depth_tlist x tlist
and depth_tlist x = function
    [] -> raise NotFound
  | [t] -> depth x t
  | t::rest -> try depth x t
               with NotFound -> depth_tlist x rest

(* ESERCIZIO 2 ESAME LUGLIO 2018 *)
let rec parentela ntree x y =
  match ntree with
    Tr(k, []) -> 
        if x = k then depth x ntree
        else if y = k then depth y ntree
        else raise NotFound
  | Tr(k, tlist) -> 
        if x = k then (parentela_tlist ntree x y tlist) - (depth x ntree)
        else if y = k then (parentela_tlist ntree x y tlist) - (depth y ntree)
        else parentela_tlist ntree x y tlist
and parentela_tlist z x y = function
    [] -> (depth x z) + (depth y z)
  | [t] -> parentela t x y
  | t::rest ->
        try parentela t x y
        with NotFound -> parentela_tlist z x y rest


(* ESAME LUGLIO 2017 - ESERCIZIO 2 *)
type 'a ntree = Tr of 'a * 'a ntree list
type path_form = AE of string | EA of string

let value = function 
    AE x -> x
  | EA x -> x

let model = function
    AE _ -> "AE"
  | EA _ -> "EA"

let rec verify pform = function 
    Tr(l, []) -> List.mem (value pform) l
  | Tr(l, tlist) -> 
      match pform with
        AE p -> List.mem p l || verify_list pform tlist
      | EA p -> List.mem p l && verify_list pform tlist
and verify_list pform = function
    [] -> if (model pform) == "AE" then true else false
  | t::rest -> 
      if (model pform) == "AE" 
      then verify pform t && verify_list pform rest
      else verify pform t || verify_list pform rest
  
                    



