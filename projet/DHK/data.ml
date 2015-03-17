type ant = 
  | Corpse
  | Ally of state * environment
  | Enemy

and state = {
  energy : int;
  acid : int;
  max_energy : int;
  max_acid : int
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
  | Rest
  | TurnLeft
  | TurnRight
  | Zombify of string 

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

let string_of_command = function
  | Attack i -> "attack@" ^ (string_of_int i)
  | Move -> "forward"
  | Rest -> "rest"
  | TurnLeft -> "left"
  | TurnRight -> "right"
  | Zombify s -> "hack@[" ^ s ^ "]"

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

let basic_order = [Front;Left;Right;FrontLeft;FrontRight;Back;BackLeft;BackRight]

let search_first f order ant = 
  let rec aux = function
    | [] -> None
    | pos::pl -> begin
      match f (environment ant pos) with
      | None -> aux pl
      | Some res -> Some(pos,res)
    end
  in aux order
