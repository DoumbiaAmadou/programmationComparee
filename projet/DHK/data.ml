type ant = 
  | Corpse
  | Ally of state * environment
  | Enemy

and state = {
  energy : int;
  acid : int
}

and environment = (field * ant option) list

and field = 
  | Grass
  | Rock
  | Water
  | Food of food

and food =
  | Wheat
  | Jam
  | Sugar

and command = 
  | Attack of int
  | Move
  | TurnLeft
  | TurnRight
  | Zombify of program 

(*en attendant la spec*)
and program = int

type position =
  | Front
  | Back
  | Left
  | Right
  | FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  | On

let energy = function
  | Ally(s,_) -> Some s.energy
  | _ -> None

let acid = function 
  | Ally(s,_) -> Some s.acid
  | _ -> None

(* 
   Avec une fourmie orientee vers le haut,
   l'ordre des cases est comme suit 
     0 1 2
     3 4 5
     6 7 8
*)
let environment ant position = 
  let get_cell surround position = 
    let c = List.nth surround in
    match position with
    | Front -> c 1
    | Back -> c 7
    | Left -> c 3
    | Right -> c 5
    | FrontLeft -> c 0
    | FrontRight -> c 2
    | BackLeft -> c 6
    | BackRight -> c 8
    | On -> c 4
  in
  match ant with
  | Ally(_,env) -> Some (get_cell env position)
  | _ -> None
