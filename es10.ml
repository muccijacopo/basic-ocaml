(* Definizione di grafo *)
type 'a graph = ('a * 'a) list 
exception NotFound

(* Grafi di esempio *)
let grafo1 = [(1,2);(1,3);(2,4);(2,5);(3,5);(2,6);(5,7);(5,8)]
let grafo2 = [(1,2);(1,3);(3,4);(4,2)]
let grafo3 = [(1,2);(1,3);(3,4);(4,1)]

(* setadd: 'a -> 'a list -> 'a list *)
let setadd x lst = 
  if List.mem x lst then lst
  else x::lst

(* nodes: 'a graph -> 'a list *)
let rec nodes = function 
    [] -> []
  | (x,y)::rest -> setadd x (setadd y (nodes rest))

(* successori: 'a -> 'a graph -> 'a list *)
let rec successori nodo = function 
    [] -> [] 
  | (x,y)::rest -> if nodo = x then y::(successori nodo rest) 
                   else successori nodo rest 

(* oppure utilizzando le funzione di ordine superiore *)
let rec successori2 nodo grafo =
    List.map snd (List.filter (function (x,_) -> x = nodo) grafo)

(* nel caso di grafi non orientati *)
(* vicini: 'a -> 'a graph -> 'a list *)
let rec vicini nodo = function 
    [] -> [] 
  | (x,y)::rest -> 
          if x = nodo then y::vicini nodo rest
          else 
            if y = nodo then x::vicini nodo rest
            else vicini nodo rest 

(* visita in profondità (DFS) *)
(* depth_first_collect: 'a graph -> 'a -> 'a list *)
(* search: 'a list -> 'a list -> 'a list *)
let depth_first_collect graph start =
  let rec search visited = function 
    [] -> visited 
  | nodo::rest -> 
        if List.mem nodo visited then search visited rest
        else search (nodo::visited) ((successori nodo graph) @ rest)
  in search [] [start]

(* search_node: 'a graph -> 'a -> ('a -> bool) -> 'a *)
let search_node graph start p =
  let rec search visited = function 
    [] -> raise NotFound
  | node::rest -> 
        if List.mem node visited 
        then search visited rest
        else if p node then node 
             else search (node::visited) (rest @ (successori node graph))
  in search [] [start]

(* raggiungibile: 'a graph -> 'a -> 'a -> bool *)
(* raggiunbibile g start goal = vera se goal è raggiungibile da start *)
let raggiungibile g start goal =
  try goal = search_node g start ((=) goal)
  with NotFound -> false 

  (* RICERCA DI UN CAMMINO *)
(* search_path: 'a graph -> 'a -> ('a -> bool) -> 'a list *)
(* search_path grafo start p = cammino nel grafo da start fino a un nodo che soddisfa p *)

(* funzioni ausiliarie *)
(* from_node: 'a list -> 'a -> 'a list *)
(* from_node visited n = cammino da n che non passa per nodi in visited fino a un nodo che soddisfa p *)

(* ricerca a partire da una lista di nodi, vicini o successori di uno stesso nodo *)
(* from_list: 'a list -> 'a list -> 'a list *)
(* from_list visited nodes = cammino che non passa per nodi in visited e che parte da un nodo in nodes e arriva a un nodo che soddisfa p *)

let search_path grafo start p = 
  let rec from_node visited node = 
    if List.mem node visited 
    then raise NotFound
    else if p node then [node]
    else node::from_list (node::visited) (successori node grafo)
  and from_list visited = function 
    [] -> raise NotFound
  | node::rest -> 
      try from_node visited node
      with NotFound -> from_list visited rest
  in from_node [] start

(* path_max: 'a graph -> 'a -> 'a -> int -> 'a list *)
(* path_max grafo start goal max = cammino da start a goal di lunghezza massima max *)
let path_max grafo start goal max = 
  let rec from_node visited node len =
    if List.mem node visited || len > max
    then raise NotFound
    else if node = goal then [goal]
    else node::from_list (node::visited) (len+1) (successori node grafo) 
  and from_list visited len = function 
    [] -> raise NotFound
  | node::rest -> 
        try from_node visited node len
        with NotFound -> from_list visited len rest
  in from_node [] start 0

(* ESERCIZI MODULO 10 *)

(* test_connessi: 'a graph -> 'a -> 'a -> bool *)
(* test_connessi grafo start goal = true se esiste un cammino da start a goal, false altrimenti *)
let test_connessi grafo start goal =
  let rec search visited = function 
    [] -> false 
  | node::rest -> 
            if List.mem node visited
            then search visited rest 
            else node = goal ||
              search (node::visited) (rest @ (successori node grafo))
  in search [] [start]

(* esiste_ciclo: 'a graph -> 'a  -> bool *)
(* esiste_ciclo: grafo nodo = true se esiste un ciclo su nodo *)
let esiste_ciclo grafo nodo =
  let rec search visited = function 
    [] -> false 
  | n::rest ->
      if List.mem n visited 
      then search visited rest
      else n = nodo ||
        search (n::visited) (rest @ (successori n grafo))
  in search [] (successori nodo grafo)

(* ciclo: 'a graph -> 'a -> 'a list *)
(* ciclo grafo nodo = lista di nodi che formano un ciclo su nodo in grafo *)
let ciclo grafo start = 
  let rec from_node visited nodo =
    if List.mem nodo visited 
    then raise NotFound
    else if nodo = start then [nodo]
    else nodo::from_list (nodo::visited) (successori nodo grafo) 
  and from_list visited = function 
    [] -> raise NotFound
  | nodo::rest -> 
          try from_node visited nodo
          with NotFound -> from_list visited rest 
  in start::from_list [] (successori start grafo)

(* definizione di grafo alternativa: coppia di lista di nodi e lista di archi *)
type 'a graph2 =  'a list * ('a * 'a) list
let grafo4 = ([1;2;3;4;5], [(1,2);(1,3);(2,4);(4,5);(5,1)])
let grafo5 = ([1;2;3], [(1,2);(2,3);(3,1)]) (* grafo hamiltoniano *)
let grafo6 = ([1;2;3], [(3,1);(2,3);(1,2)])

(* grafo_connesso: 'a graph2 -> bool *)
(* grafo_connesso grafo = true se il grafo è connesso *)

let connessi archi start goal =
  let rec search visited = function
    [] -> false
  | n::rest -> 
        if List.mem n visited
        then search visited rest
        else n = goal || search (n::visited) (rest @ (vicini n archi))
  in search [] [start]

let grafo_connesso (nodi, archi) =
  let rec aux = function 
    [] | [_] -> true 
  | n1::n2::rest -> (connessi archi n1 n2) && aux (n2::rest)
  in aux nodi

(* esercizio 6 *)
(* cammino: 'a graph2 -> 'a list -> 'a -> 'a -> 'a list *)
(* cammino grafo lista start goal = cammino da start a goal in grado che passa solo ed esattamente una volta per i nodi di lista *)
let cammino (nodi, archi) lista start goal =
  let rec from_node node lst =
    if [node] = lst && node = goal 
    then [node] (* cammino trovato *)
    else if List.mem node lst 
    then node::from_list (List.filter ((<>)node) lst) (successori node archi)
    else raise NotFound
  and from_list lst = function 
    [] -> raise NotFound
  | n::rest -> 
        try from_node n lst 
        with NotFound -> from_list lst rest 
  in from_node start lista

(* hamiltoniano: 'a graph2 -> 'a -> 'a list *)
(* hamiltoniano: grafo start = riporta il ciclo, se esiste, di start che tocca tutti i nodi del grafo *)
let hamiltoniano (nodi, archi) = 
  let start = List.hd nodi in
    let rec from_node nodo nodi =
      if not(List.mem nodo nodi)
      then raise NotFound
      else 
        if nodi = [start] && nodo = start then [nodo]
        else nodo::from_list (List.filter ((<>) nodo) nodi) (successori nodo archi)
    and from_list nodi = function 
      [] -> raise NotFound
    | nodo::rest -> 
          try from_node nodo nodi
          with NotFound -> from_list nodi rest

    in start::from_list nodi (successori start archi)
    
(* esercizio 8 *)
(* connessi_in_glist: 'a graph list -> 'a -> 'a -> bool *)
let test_connessi grafo start goal =
let rec cammino visited = function
        [] -> false
      | nodo::rest -> 
              if List.mem nodo visited
              then cammino visited rest 
              else nodo = goal || 
                cammino (nodo::visited) (rest @ (successori nodo grafo))
      in cammino [] [start]

let connessi_in_glist lgrafi a b = 
  a <> b && 
  List.exists (function g -> test_connessi g a b || test_connessi g b a) lgrafi

(* esercizio 9 *)
(* cammino_con_nodi: 'a graph -> 'a -> 'a list -> 'a list *)
(* cammino_con_nodi grafo start lista = riporta il cammino, se esiste, da start che passa per tutti i nodi di lista *)
let cammino_con_nodi grafo start lista =
  let rec from_node visited lista node =
    if List.mem node visited 
    then raise NotFound
    else if [node] = lista then [node]
    else node::from_list (node::visited) (List.filter((<>) node) lista) (successori node grafo)
  and from_list visited lista = function 
    [] -> raise NotFound
  | node::rest ->   
        try from_node visited lista node
        with NotFound -> from_list visited lista rest
  in from_node [] lista start

(* esercizio 11 *)
(* cammino_di_primi: int graph -> int -> int list *)
let is_primo n = 
    let rec aux = function 
      | 1 -> true
      | m ->  (n mod m) <> 0 && aux (m-1)
    in n = 1 || aux (n-1)

let esiste_cammino_di_primi grafo start goal =
  let rec search visited = function
    [] -> true 
  | n::rest -> 
      if List.mem n visited 
      then search visited rest
      else n = goal && is_primo n || is_primo n && search (n::visited) (rest @ (successori n grafo))
  in search [] [start]

let cammino_di_primi grafo start goal =
  let rec from_node node visited =
    if List.mem node visited || not(is_primo node)
    then raise NotFound
    else 
      if node = goal then [goal]
      else node::from_list (node::visited) (successori node grafo)
  and from_list visited = function 
    [] -> raise NotFound
  | n::rest -> 
        try from_node n visited
        with NotFound -> from_list visited rest 
  in from_node start []

(* esercizio 13 *)
(* path_n_p: 'a graph -> ('a -> bool) -> 'int -> 'a -> 'a list *)
let pari n = n mod 2 = 0

let path_n_p grafo p n start =
  let rec from_node node np visited =
    if List.mem node visited then 
    raise NotFound
    else 
      if p node && np = 1 then [node]
      else node::from_list (node::visited) (if p node then np-1 else np) (successori node grafo)
  and from_list visited np = function 
    [] -> raise NotFound
  | n::rest -> 
        try from_node n np visited
        with NotFound -> from_list visited np rest
  in from_node start n []

(* ESERCIZIO 2 ESAME FEBBRAIO 2018 *)
type 'a money = ('a * int) list
(* safe_path: 'a graph -> 'a money -> 'a -> 'a -> int -> 'a list *)
(* safe_path: grafo wallet start goal init = cammino da start a goal senza scenedere sotto 0 *)
let safe_path grafo wallet start goal init =
  let rec from_node coin node visited =
    if List.mem node visited then raise NotFound
    else let k = try List.assoc node wallet
                 with _ -> 0
         in if (coin + k) < 0 then raise NotFound
            else if node = goal then [node]
            else node::from_list (coin+k) (node::visited) (vicini node grafo) 
  and from_list coin visited = function
      [] -> raise NotFound
    | n::rest -> 
        try from_node coin n visited
        with NotFound -> from_list coin visited rest
  in from_node init start []

let grafo5 = [('A','B');('A','D');('A','C');('B','E');('E','G');('G','F');('F','D');('F','C')]
let wallet = [('C',-7);('D',-15);('F',3);('G',-5)]


(*---------------------------*)



(* ESAME SETTEMBRE 2018 - ESERCIZIO 3 *)
(* sorted_path: 'a graph -> 'a -> 'a -> 'a list *)
(* sorted_path grafo start goal = riporta un cammino ordinato da start a goal *)
let sorted_path grafo start goal = 
  let rec from_node nodo_prec nodo visited =
    if List.mem nodo visited then raise NotFound
    else
      if nodo_prec <= nodo then 
        if nodo = goal then [goal]
        else nodo::from_list nodo (nodo::visited) (successori nodo grafo) 
      else raise NotFound
  and from_list nodo_prec visited = function
    [] -> raise NotFound
  | n::rest -> 
      try from_node nodo_prec n visited 
      with NotFound -> from_list nodo_prec visited rest
  in from_node start start [] 

(* ------------------------- *)
(* ESAME FEBBRAIO 2017 *)
type 'a option = None | Some of 'a
(* whichpath: 'a graph -> 'a option list -> 'a -> 'a -> 'a list *)
let whichpath grafo optionlist start goal =
  let rec from_node optionlist node visited =
    match optionlist with
    [] -> if List.mem node visited then raise NotFound
          else 
            if node = goal then [node]
            else node::from_list [] (node::visited) (successori node grafo)
  | (None)::rest -> node::from_list rest (node::visited) (successori node grafo)
  | (Some x)::rest -> 
          if node = x then node::from_list rest (node::visited) (successori node grafo)
          else raise NotFound
  and from_list optionlist visited = function
    [] -> raise NotFound
  | n::rest -> try from_node optionlist n visited
               with _ -> from_list optionlist visited rest
  in from_node optionlist start []
  
let optionlist = [None; Some 3; None]

(* ESAME SETTEMBRE 2017 - ESERCIZIO 2 *)
let is_pari x = x mod 2 = 0

let alt_path grafo start goal =
  let rec from_node nodo_prec nodo visited =
    if List.mem nodo visited then raise NotFound
    else 
      if is_pari nodo_prec then (* il nuovo nodo deve essere dispari *)
        if not (is_pari nodo) then
          if nodo = goal then [nodo] 
          else nodo::from_list nodo (nodo::visited) (successori nodo grafo) 
        else raise NotFound
      else (* il nuovo nodo deve essere pari *)
        if is_pari nodo then 
          if nodo = goal then [nodo]
          else nodo::from_list nodo (nodo::visited) (successori nodo grafo)
        else raise NotFound
  and from_list nodo visited = function 
    [] -> raise NotFound
  | n::rest -> try from_node nodo n visited
               with NotFound -> from_list nodo visited rest
  in start::from_list start [] (successori start grafo)