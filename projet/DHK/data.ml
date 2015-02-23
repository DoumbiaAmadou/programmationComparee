type ant = 
  | Corpse
  | Ally of state * environment
  | Ennemy

and state = {
  energy : int;
  acid : int
}

and environmnent = (field * ant option) list

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

