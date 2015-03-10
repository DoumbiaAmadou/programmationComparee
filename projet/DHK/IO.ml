open Nethttp_client.Convenience

exception HttpPostError
exception HttpGetError
  
(* Prefix of the server URL *)
let prefix_url = "http://yann.regis-gianas.org/antroid/"

(* The current version of the API *)
let version = "0"

(* The first part url of the server *)
let url = prefix_url ^ version

(* [concat_url p s] return the concatenation between [p] and [s]. *)
let concat_url p s = Printf.sprintf "%s/%s" p s

(* [register_user l p] try to add a new user with the login [l] and the 
   password [p]. Raise an HttpPostError exception if the request fails. *)
let register_user l p =
  let url = concat_url url "register" in 
  let post_params = [("login",l); ("password",p)] in
  try 
    let json_string = http_post url post_params in
    let json = Yojson.Basic.from_string json_string in
    let open Yojson.Basic.Util in
    let status = json |> member "status" |> to_string in
    match status with
    | "error" -> false
    | "completed" -> true
    | _ -> assert false 
  with _ -> raise HttpPostError

(* [auth_user l p] make the authentication of a user having the login [l] and 
   the password [p]. Raise an HttpPostError exception if the request fails.*)
let auth_user l p =
  let url = concat_url url "auth" in
  let post_params = [("login",l); ("password",p)] in
  try 
    let json_string = http_post url post_params in
    let json = Yojson.Basic.from_string json_string in
    let open Yojson.Basic.Util in
    let status = json |> member "status" |> to_string in
    match status with 
    | "error" -> false
    | "completed" -> true
    | _ -> assert false 
  with _ -> raise HttpPostError

(* [is_between min max v] check if the value [v] is between [min] and [max]. *)
let is_between min max v = v >= min && min <= max

(* [check_users_id users] test if [users] is well formed. *)
let check_users_id users =
  let regexp = Str.regexp "^\\([0-9A-Za-z]+\\)\\(,[0-9A-Za-z]+\\)*$" in
  users = "all" || Str.string_match regexp users 0

(* [encode str] replace all space character in [space] by the character "+" *)
let encode str = Str.global_replace (Str.regexp " ") "+" str

exception BadFormatArgument
  
(* [create_new_game users teaser pace nb_turn nb_ant_per_player nb_player
   minimal_nb_player initial_energy initial_acid] try to create a new game.
   Raise an HttpGetError exception if the request fails. *)
let create_new_game ~users ~teaser ~pace ~nb_turn ~nb_ant_per_player
    ~nb_player ~minimal_nb_player ~initial_energy ~initial_acid =
  let predicates =
    [ check_users_id users;
      is_between 1 100 pace;
      is_between 1 10000 nb_turn;
      is_between 1 42 nb_ant_per_player;
      is_between 1 42 nb_player;
      is_between 1 nb_player minimal_nb_player;
      is_between 1 1000 initial_energy;
      is_between 1 1000 initial_acid
    ] in
  let b = List.fold_left (fun acc p -> acc && p) true predicates in
  if b then 
    let url = concat_url url "create" in
    let url = url ^ "?users=" ^ users ^
              "&teaser=" ^ encode(teaser) ^
              "&pace=" ^ (string_of_int pace) ^
              "&nb_turn=" ^ (string_of_int nb_turn) ^
              "&nb_ant_per_player=" ^ (string_of_int nb_ant_per_player) ^
              "&nb_player=" ^ (string_of_int nb_player) ^
              "&minimal_nb_player=" ^ (string_of_int minimal_nb_player) ^
              "&initial_energy=" ^ (string_of_int initial_energy) ^
            "&initial_acid=" ^ (string_of_int initial_acid) in
    try
      Printf.printf "%s\n" url;
      let s = http_get url in
      Printf.printf "%s\n" s
    with _ -> raise HttpGetError
  else raise BadFormatArgument
    

(* [do_game_action i action] will send a request, doing the action [action], 
   to the game having the id [i]. Raise an HttpGetError if the request fails. *)
let do_game_action i action =
  let url = concat_url url action in
  let url = url ^ "?id=" ^ i  in
  try
    let s = http_get url in
    Printf.printf "%s\n" s
  with _ -> raise HttpGetError

(* [destory_game i] try to destroy the game having the id [i].*)
let destroy_game i = do_game_action i "destroy"

(* [join_game i] try to join the game having the id [i]. *)
let join_game i = do_game_action i "join"

(* [log_game i] try to get the log of the game having the id [i]. *)
let log_game i = do_game_action i "log"

(* [get_game_status i] try to get the status of the game having the id [i]. *)
let get_game_status i = do_game_action i "status"

(* [get_current_games] get the list of all visibles games.*)
let get_current_games () =
  let url = concat_url url "games" in
  try
    let s = http_get url in
    Printf.printf "%s\n" s
  with _ -> raise HttpGetError





