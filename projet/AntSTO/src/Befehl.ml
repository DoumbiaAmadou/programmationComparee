(** Fichier contenant les ordres/commandes que les fourmis doivent realiser *)

open Struktur

type cell_front_content =
  | Rock
  | Water
  | Grass
  | Food
  | UndifinedSee

and ant_front =
  | No
  | Controlled
  | Zombie
  | Dead
  | UndifinedSeeAnt

let type_of_see = function
  | 0 -> Rock
  | 1 -> Water
  | 2 -> Grass
  | 3 -> Food
  | _ -> UndifinedSee

let type_of_see_ant = function
  | 0 -> No
  | 1 -> Controlled
  | 2 -> Zombie
  | 3 -> Dead
  | _ -> UndifinedSeeAnt

let get_command ant_id see_value see_ant_value : string =
  match see_ant_value with
  | No         ->
     begin
       match see_value with
       | Rock  -> string_of_ant_command ant_id Rest (* Gerade Gehen Sie nicht *)
       | Water -> string_of_ant_command ant_id Forward
       | Grass -> string_of_ant_command ant_id Forward (* ??? *)
       | Food  -> string_of_ant_command ant_id Forward
       | _     -> "" (* Nicht von der Spezifikation definiert *)
     end
  | Controlled -> string_of_ant_command ant_id string_of_command(Attack 1)
  (* ZU TUN : passen Sie den Wert *)
  | Zombie     -> string_of_ant_command ant_id Rest (* Gerade Gehen Sie nicht *)
  | Dead       -> string_of_ant_command ant_id Hack(["fork"])
  (* ZU TUN : passen Sie den Wert *)
  | _          -> "" (* Nicht von der Spezifikation definiert *)
