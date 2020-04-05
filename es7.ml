type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int 

let gira = function 
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su 

let avanti (x,y, dir) n =
    match dir with
    Su -> (x, y+n, dir)
  |  Destra -> (x+n, y, dir)
  |  Giu -> (x, y-n, dir)
  | Sinistra -> (x-n, y, dir)


let sposta (x,y, dir) action =
    match action with
    Gira -> (x,y, gira dir)
  | Avanti n -> avanti (x, y, dir) n  


let rec esegui pos = function
    [] -> pos
  | x::rest -> esegui (sposta pos x) rest
  
