type ant = Corpse | Ally of state * environment | Enemy
           
and state = { energy : int; acid : int; max_energy : int; max_acid : int; }
and environment = (field * ant option) list
and field = Grass | Rock | Water | Food of food
and food = Wheat | Jam | Sugar
and command =
    Attack of int
  | Move
  | Rest
  | TurnLeft
  | TurnRight
  | Zombify of string
type position =
    Front
  | Back
  | Left
  | Right
  | FrontLeft
  | FrontRight
  | BackLeft
  | BackRight
  | On
    
val field_of_string : string -> field
val ant_of_string : string -> ant 
val energy : ant -> int option
val acid : ant -> int option
val string_of_command : command -> string
val environment : ant -> position -> (field * ant option) option
val basic_order : position list
val search_first :
  ((field * ant option) option -> 'a option) ->
  position list -> ant -> (position * 'a) option
