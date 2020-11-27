(* ESERCIZI ALBERI BINARI *)
exception NotBalanced
exception NotFound
(* definizione di albero binario *)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let tree1 = Tr(1,Tr(2,Tr(4,Empty,Empty),
Empty),
Tr(3,Tr(5,Tr(6,Empty,Empty),
Tr(7,Empty,Empty)),
Empty))

let rec size = function 
    Empty -> 0
  | Tr(_, t1, t2) -> 1 + size t1 + size t2

let rec reflect = function
    Empty -> Empty
  | Tr(x, t1, t2) -> Tr(x, reflect t2, reflect t1)

let fulltree n =
  let rec aux n root =
    if n = 0 then Empty
    else  
      Tr(root, aux (n-1) (root*2), aux (n-1) (root*2+1))
  in aux n 1

(* ALTEZZA di un albero binario *)
let rec height = function
    Empty -> -1
  | Tr(_, t1, t2) -> 1 + max (height t1) (height t2)
(*-----------------------------*)

let rec tree_exists p = function
    Empty -> false 
  | Tr(x, t1, t2) -> p x || tree_exists p t1 || tree_exists p t2

(* lista contenente tutti i nodi dell'albero *)
let rec raccogli = function 
    Empty -> []
  | Tr(x, t1, t2) -> x::((raccogli t1) @ (raccogli t2))

let rec raccogli2 t = 
  let rec aux result = function 
      Empty -> result
    | Tr(x, t1, t2) -> aux (aux (x::result) t1) t2
  in aux [] t
(* --------------------- *)

(* lista contenente tutte le foglie dell'albero *)
let rec foglie = function 
    Empty -> []
  | Tr(x, Empty, Empty) -> [x]
  | Tr(_, t1, t2) -> (foglie t1) @ (foglie t2)

(* lista dei nodi che hanno un solo figlio *)
let rec nodi_unico_figlio = function 
    Empty -> []
  | Tr(x, t, Empty) | Tr(x, Empty, t) -> x::(nodi_unico_figlio t)
  | Tr(_, t1, t2) -> nodi_unico_figlio t1 @ nodi_unico_figlio t2

(* Cammino dalla radice fino ad una foglia x, se esiste *)
let rec path_to x = function
    Empty -> raise NotFound
  | Tr(y,Empty,Empty) ->
      if y=x then [x]
      else raise NotFound
  | Tr(y,t1,t2) -> (* backtracking *)
      y::(try path_to x t1
          with NotFound -> path_to x t2)
(* ---------------------- *)

(* path p tree = cammino dalla radice ad una foglia con tutti i nodi che non rispettano p *)
let rec path p = function 
    Empty -> raise NotFound
  | Tr(x, Empty, Empty) -> if not (p x) then [x] else raise NotFound
  | Tr(x, t1, t2) -> 
        if not (p x) then
        x::(try path p t1
            with NotFound -> path p t2)
        else raise NotFound
(* -------------------- *)

(* VISITA con RISULTATO PARZIALE (es. conteggio di etichette) *)

(* add x lista = lista che si ottiene sostiuendo la coppia (y,n) con (y, n+1) se tale coppia esiste *)
let rec add x = function 
    [] -> [(x,1)]
  | (y, n)::rest -> if x = y then (y, n+1)::rest
                    else (y,n)::add x rest
let count_etichette t =
  let rec aux result = function 
      Empty -> result
    | Tr(x, t1, t2) -> aux (aux (add x result) t1) t2
  in aux [] t

(* ------------------ *)
(* Test albero BILANCIATO *)
let rec balanced = function
    Empty -> true;
  | Tr(_, t1, t2) -> abs(height t1 - height t2) <= 1 && balanced t1 && balanced t2

(* Versione alternativa meno costosa *)
let balanced t =
  let rec aux = function
    Empty -> -1
  | Tr(_, t1, t2) -> 
                      let k1 = aux t1
                      and k2 = aux t2 
                      in if abs(k1 - k2) <= 1 
                        then 1 + max k1 k2
                      else raise NotBalanced 
  in try let _ = aux t in true 
  with NotBalanced -> false
(* ----------------------------- *)

let rec preorder = function
    Empty -> []
  | Tr(x, t1, t2) -> x::(preorder t1 @ preorder t2)


let rec postorder = function
    Empty -> []
  | Tr(x, t1, t2) -> (postorder t1) @ ((postorder t2) @ [x])


let rec inorder = function 
    Empty -> []
  | Tr(x, t1, t2) -> (inorder t1) @ (x::(inorder t2))

(* take : int -> 'a list -> 'a list *)
let rec take n = function
    [] -> []
  | x::xs -> if n<=0 then []
             else x::take (n-1) xs

(* drop : int -> 'a list -> 'a list *)
let rec drop n = function
    [] -> []
  | x::xs -> if n<=0 then x::xs
             else drop (n-1) xs;;

let rec balpreorder = function
    [] -> Empty
  | x::xs -> 
      let k = (List.length xs)/2
      in Tr(x, balpreorder (take k xs), balpreorder (drop k xs))

(* esercizio 3 *)
let rec foglie_in_lista lst = function 
    Empty -> true 
  | Tr(x, Empty, Empty) -> List.mem x lst
  | Tr(_, t1, t2) -> foglie_in_lista lst t1 && foglie_in_lista lst t2

(* esercizio 4 *)
let rec num_foglie = function 
    Empty -> 0
  | Tr(_, Empty, Empty) -> 1
  | Tr(_, left, right) -> num_foglie left + num_foglie right 

let rec segui_bool lst tree = 
  match (tree, lst) with 
    (Empty, _) -> failwith "errore albero vuoto" 
  | (Tr(x,_,_),[]) -> x
  | (Tr(_, t1, t2), y::rest) -> if y then segui_bool rest t1
                                else segui_bool rest t2


let rec foglia_costo = function 
    Empty -> failwith "errore"
  | Tr(x, Empty, Empty) -> (x, x)
  | Tr(x, left, Empty) -> 
        let (y, c) = foglia_costo left 
        in (y, c+x) 
  | Tr(x, Empty, right) -> 
        let (y, c) = foglia_costo right
        in (y, c+x)
  | Tr(x, left, right) -> 
        let (fleft, cleft) = foglia_costo left
        and (fright, cright) = foglia_costo right 
        in if (cright > cleft) then (fright, cright+x) 
            else (fleft, cleft+x)

let rec foglie_costi = function 
    Empty -> []
  | Tr(x, Empty, Empty) -> [(x,x)]
  | Tr(x, left, right) -> 
      List.map (function (y,c) -> (y, c+x)) ((foglie_costi left) @ (foglie_costi right))

(* ESAME SETTEMBRE 2018 - ESERCIZIO 2 *)
(* sorted_branch: 'a tree -> 'a -> 'a list *)
(* sorted_branch tree x = riporta un ramo ordinato dalla radice fino ad una foglia x *)
let rec sorted_branch tree x =
  let rec aux nodo_prec x = function
    Empty -> raise NotFound
  | Tr(nodo, Empty, Empty) -> 
        if nodo = x then [nodo]
        else raise NotFound
  | Tr(nodo, t1, t2) -> 
        if nodo > nodo_prec then nodo::(try aux nodo x t1 
                                        with NotFound -> aux nodo x t2)
        else raise NotFound
  in aux 0 x tree