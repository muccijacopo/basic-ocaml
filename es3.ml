
(* Liste associative *)
let dizionario = [("Jacopo", 20);("Sara", 21);("Alessia", 49)]

let inserisci k v dizionario = 
    (k,v)::dizionario

let rec cancella k = function
    [] -> []
  | (y,z)::rest ->
        if k = y then
            cancella k rest
        else
            (y,z)::cancella k rest

let rec sumof = function
    [] -> 0
  | x::rest -> x + sumof rest


let double x = x * 2

(* ultime_cifre: int -> int * int *)
let ultime_cifre x =
  let y = abs x in 
    ((y/10) mod 10, y mod 10)

(* data: int * string -> bool *)
let data (d,m) =
  d > 0 &&
  match m with
   "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" -> d <= 31 
  | "febbraio" -> d <= 28
  | "novembre" | "aprile" | "giugno" | "settembre" -> d <= 30
  | _ -> false


(* somma_ore: (int * int) -> (int * int) -> int * int *)

let verifica (h,m) =
  h >= 0 && h < 24 && m >= 0 && m < 60

let somma_minuti m1 m2 = 
  let risultato = m1 + m2
  in (risultato / 60, risultato mod 60 )


let somma_ore (h1, m1) (h2, m2) =
  if verifica (h1, m1) && verifica (h2, m2) then 
    let (h3, m3) = somma_minuti m1 m2 
    in ((h1 + h2 + h3) mod 24, m3)
  else failwith "Formato scorretto"


let read_max () =
  let rec aux n =
    try (* se non si immette un numero, viene sollevata un'eccezione
	   e il ciclo termina, riportando n *)
      let k = read_int()
      in aux (max n k)
    with _ -> n
  in try aux (read_int())
     with _ -> failwith "Sequenza vuota" 

(* read_max_min: unit -> int * int *)
let read_max_min () =
  let rec aux massimo minimo =
    try 
      let k = read_int()
      in aux (max  massimo k ) (min minimo k)
    with _ -> (massimo, minimo)
  in
  try let n = read_int() in
    aux n n 
  with _ -> raise (Failure "Sequenza vuota") 

(* tutti_minori: int -> bool *)
let rec tutti_minori n =
  try 
    let k = read_int()
    in tutti_minori n && k < n
  with _ -> true

(* occorre: int -> bool *)
let rec occorre n =
  try
    let k = read_int()
    in occorre n || k = n 
  with _ -> false

(* num_di_stringhe: unit -> int *) 
let rec num_di_stringhe () =
    if read_line() = "" then 0
    else 1 + num_di_stringhe()


(* stringa_max: unit -> string *)
let rec stringa_max () =
  let nuova = read_line() 
  in
    if nuova = "" then ""
    else
      let s = stringa_max() in 
      if String.length s > String.length nuova
      then s
      else nuova 
      
(* sumbetween: int -> int -> int *)
let rec sumbetween n m =
  if n > m then 0
  else 
    n + sumbetween (n+1) m

(* sumto: int -> int *)
let rec sumto = function
    0 -> 0
  | n -> n + sumto (n-1)

(* power: int -> int -> int *)
let rec power n = function
    0 -> 1 
  | k -> n * power n (k-1)

