open Nethttp_client.Convenience
open Yojson.Basic.Util
       
exception HttpPostError
exception HttpGetError

type cookie = string

type game_identifier = string
  
let string_of_cookie cookie = cookie
  
let string_of_gameid gid = gid

let gameid_of_string gid = gid

let prefix_url = "http://yann.regis-gianas.org/antroid/"

(** The current version of the API *)
let version = "0"

(** The first part url of the server *)
let url = prefix_url ^ version

(** [concat_url p s] return the concatenation between [p] and [s]. *)
let concat_url p s = Printf.sprintf "%s/%s" p s

let make_get_url (prefix : string) (l : (string * string) list) : string =
  match l with
  | [] -> assert false
  | (field, value) :: tail  -> 
    let url = prefix ^ field ^ "=" ^ value in 
    List.fold_left (fun url (field,value) ->
        url ^ "&" ^ field ^ "=" ^ value) url tail

let request_status json =
  json |> member "status" |> to_string = "completed"

let init_conn url =
  let writer_callback buf s =
    Buffer.add_string buf s;
    String.length s  in

  let buf = Buffer.create 10000 in 
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let connection = Curl.init () in 
  Curl.set_url connection url;
  Curl.set_verbose connection false;
  Curl.set_writefunction connection (writer_callback buf);
  buf, connection

let register_user l pwd =
  let url = concat_url url "register" in
  let post_params = ("login=" ^ l ^ "&password=" ^ pwd) in
  try
    let buf, conn = init_conn url in
    Curl.set_postfields conn post_params;
    Curl.set_postfieldsize conn(String.length post_params);
    Curl.perform conn;
    Yojson.Basic.from_string (Buffer.contents buf) 
    |> request_status 
  with _ -> raise HttpPostError

let auth_user l p =
  let url = concat_url url "auth" in
  let post_params = ("login=" ^ l ^ "&password=" ^ p) in
  let buf, conn = init_conn url in
  Curl.set_postfields conn post_params;
  Curl.set_postfieldsize conn (String.length post_params);
  Curl.set_verbose conn true;
  Curl.set_cookiefile conn "";
  Curl.perform conn;
  let json = Yojson.Basic.from_string (Buffer.contents buf) in
  let status = request_status json in 
  if status
  then
    let lcookie = Str.split (Str.regexp "\t") (
        List.hd (Curl.get_cookielist conn)) in
    match List.rev lcookie with
    | [] -> assert false
    | [x] -> assert false
    | x :: y :: _ -> y  ^ "=" ^ x  end 
else failwith "Unsuccesfuly request"

exception BadFormatArgument

let create_new_game ~users ~teaser ~pace ~nb_turn ~nb_ant_per_player
    ~nb_player ~minimal_nb_player ~initial_energy ~initial_acid ~cookie =

  (**[is_between min max v] check if the value [v] is between [min] and [max].*)
  let is_between min max v = v >= min && min <= max in

  (** [check_users_id users] test if [users] is well-formed. **)
  let check_users_id users =
    let regexp = Str.regexp "^\\([0-9A-Za-z]+\\)\\(,[0-9A-Za-z]+\\)*$" in
    users = "all" || Str.string_match regexp users 0 in

  (* [encode str] replace all space character in [space] by the character "+" *)
  let encode str = Str.global_replace (Str.regexp " ") "+" str in

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
  if b
  then
    let url = concat_url url "create?" in
    let get_params =
      [ "users", users;
        "teaser", encode teaser;
        "pace", string_of_int pace;
        "nb_turn", string_of_int nb_turn;
        "nb_ant_per_player", string_of_int nb_ant_per_player;
        "nb_player", string_of_int nb_player;
        "minimal_nb_player", string_of_int minimal_nb_player;
        "initial_energy", string_of_int initial_energy;
        "initial_acid", string_of_int initial_acid ] in

    let url = make_get_url url get_params in
    let buf, conn = init_conn url in
    (* Setting the cookie to make possible the creation of a game *)
    Curl.set_cookie conn cookie;
    Curl.perform conn;
    Printf.printf "buf_create_game = %s\n" (Buffer.contents buf);
    (* Translate the request content in a JSON format*)
    let json = Yojson.Basic.from_string (Buffer.contents buf) in
    (* Getting the status of the request *)
    let status = request_status json in 
    if status
    then 
      (* Return the ID of the created game *)
      json |> member "response" |> member "identifier" |> to_string
    else failwith "Error request"
  else raise BadFormatArgument

let destroy_game cookie id =
  let url = concat_url url "destroy?" in
  let get_params = ["id", id] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Yojson.Basic.from_string (Buffer.contents buf)
  |> request_status 

(** A game description should be contains his identifier, his creator and his
    his teaser. *)

type game_description = string * string * string                        
(* TODO : récupérer les identifiers, teasers et creator (utile pour la
   partie js_of_ocaml  *)
let show_games cookie =
  let url = concat_url url "games" in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.set_verbose conn true;
  Curl.perform conn;
  let json = Yojson.Basic.from_string (Buffer.contents buf) in
  let status = request_status json in 
  if status
  then 
    let games = json |> member "response" |> member "games" in
      [games]
      |> filter_member "game_description"
      |> flatten
      |> List.iter (fun g ->
          Printf.printf "%s\n" (g |> member "identifier" |> to_string));
      []
  else failwith "Error request"


let join_game cookie id =
  Printf.printf "join game cookie = %s\n" cookie;
  let url = concat_url url "join?" in
  let get_params = ["id", id] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Printf.printf "buf = %s\n" (Buffer.contents buf);
  Yojson.Basic.from_string (Buffer.contents buf) 
  |> request_status 

let logout cookie =
  let url = concat_url url "logout" in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Yojson.Basic.from_string (Buffer.contents buf) 
  |> request_status 

let play cookie id cmds =
  let url = concat_url url "play?" in
  let get_params = [("id", id); ("cmds", cmds)] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Printf.printf " Play = %s\n" (Buffer.contents buf)

let game_status cookie id =
  let url = concat_url url "status?" in
  let get_params = [("id", id)] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Printf.printf "Game status = %s\n" (Buffer.contents buf)






