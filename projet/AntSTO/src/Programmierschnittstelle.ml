(**
 * Module représentant les fonctions de l'API.
 *
 * Les fonctions ci-après renvoient un [Gemeinsam.either] encodant
 * l'objet JSON renvoyé ou une erreur.
 *)
open Gemeinsam
open Kommunikation

(** @return La description de l'API. *)
let api () =
  call_api "api" `Get []

(** @return Une valeur indiquant le succès de l'opération. *)
let auth user password =
  call_api "auth" `Post ["user", user; "password", password]

let create us t p nbt nbapp nbpl minpl inite initac =
  let args = ["users", String.concat "," us;
              "teaser", t;
              "pace", string_of_int p;
              "nb_turn", string_of_int nbt;
              "nb_ant_per_player", string_of_int nbapp;
              "nb_player", string_of_int nbpl;
              "minimal_nb_player", string_of_int minpl;
              "initial_energy", string_of_int inite;
              "initial_acid", initac] in
  call_api "create" `Get args

let destroy id =
  call_api "destroy" `Get ["id", id]

let games () =
  call_api "games" `Get []

let join id =
  call_api "join" `Get ["id", id]

let log id =
  call_api "log" `Get ["id", id]

let logout () =
  call_api "logout" `Get []

let play id cmds =
  let cmds = List.map Struktur.string_of_ant_command cmds
             |> String.concat "," in
  call_api "play" `Get ["id", id; "cmds", cmds]

let register user password =
  call_api "register" `Post ["user", user; "password", password]

let shutdown id =
  call_api "shutdown" `Get ["id", id]

let status id =
  call_api "status" `Get ["id", id]

let whoami () =
  call_api "whoami" `Get []
