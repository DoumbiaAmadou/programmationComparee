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
    Curl.set_postfieldsize conn (String.length post_params);
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
  Curl.set_cookiefile conn "";
  Curl.set_verbose conn true;
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
    | x :: y :: _ -> y  ^ "=" ^ x  
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
  Curl.perform conn;
  let json = Yojson.Basic.from_string (Buffer.contents buf) in
  let status = request_status json in 
  if status
    then
  (* then *)
  (*   let games =  *)
  (*     json  *)
  (*     |> member "response" *)
  (*     |> filter_member "games" *)
  (*     |> flatten in  *)
  (*   List.iter (fun g -> *)
  (*       g  *)
  (*       |> member "identifier" *)
  (*       |> to_string *)
  (*       |> Printf.printf "%s\n") games; *)
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

type ant_obs = {
  x : int;
  y : int;
  dx: int;
  dy: int;
  brain: string;
  id: int;
  energy: int;
  acid:int;
  field : cell list;
  ant_loc : ant_location list;
}

and cell = {
  cx : int;
  cy : int;
  content : string
}

and ant_location = {
  lx : int;
  ly : int;
  ldx : int;
  ldy : int;
  lbrain : string;
}

let find_cell (x,y) r = if (x,y) = (r.cx, r.cy) then Some r.content else None

let ant_loc (x,y) r = if (x,y) = (r.lx, r.ly) then Some r.lbrain else None
    
let to_ant_obs : Yojson.Basic.json -> ant_obs list = fun json -> []
  
let orientation x y dx dy position =
  let l = [ (0,1);
            (1,1);
            (1,0);
            (1,-1);
            (0,-1);
            (-1,-1);
            (-1,0);
            (-1,1) ] in
  
  let index l v = 
    let rec index_aux l v i = match l with 
      | [] -> (-1)
      | x :: tl -> if x = v then i else index_aux tl v (i+1) in
    index_aux l v 0 in
  
  let get_orientation ind n = List.nth l (ind + n mod 8) in
  
  let aux orient =
    let ind = index l orient in
    let open Data in
    function
    | Front -> orient
    | Back -> get_orientation ind 4 
    | Left -> get_orientation ind (-2)
    | Right -> get_orientation ind (2)
    | FrontLeft -> get_orientation ind (-1)
    | FrontRight -> get_orientation ind (1)
    | BackRight -> get_orientation ind 3
    | BackLeft -> get_orientation ind (-3)
    | On -> 0,0 in

  let (dx, dy) = aux (dx,dy) position in
  dx+x, dy+y
            

let max_state cookie = (0,0)

let rec search_data l f = match l with
  | [] -> None
  | x  :: tail -> match f x with
    | Some s as s' -> s'
    | None -> search_data tail f                

let env ant_obs =
  let open Data in 
  let positions = [FrontLeft;Front; FrontRight;
                   Left; On; Right;
                   BackLeft; Back; BackRight] in
  
  let env_item lcell lant position =
    match search_data lcell (find_cell position),
          search_data lant (ant_loc position) with
    | None, _ -> assert false 
    | Some c, None -> (Data.field_of_string c, None)
    | Some c, Some lant -> (Data.field_of_string c,
                            Some (Data.ant_of_string lant)) in

  List.map (fun p ->
      let pos = orientation ant_obs.x ant_obs.y ant_obs.dx ant_obs.dy p in 
      env_item ant_obs.field ant_obs.ant_loc pos) positions
  
let data_from_json (menergy, macid) json = 
  let ant_obs_l = to_ant_obs json in
  List.map (fun r ->
      let state : Data.state = {
        Data.energy = r.energy;
        Data.acid = r.acid;
        Data.max_energy = menergy;
        Data.max_acid = macid
      } in
      r.id, Data.Ally (state, env r)) ant_obs_l

  
  let play cookie id cmds = 
  let url = concat_url url "play?" in
  let get_params = [("id", id); ("cmds", cmds)] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.set_verbose conn true;
  Curl.perform conn;
  Printf.printf "buf = %s\n" (Buffer.contents buf);
  let json = Yojson.Basic.from_string (Buffer.contents buf) in
  let (menergy, macid) = max_state cookie in 
  data_from_json (menergy, macid) json

let game_status cookie id =
  let url = concat_url url "status?" in
  let get_params = [("id", id)] in
  let url = make_get_url url get_params in
  let buf, conn = init_conn url in
  Curl.set_cookie conn cookie;
  Curl.perform conn;
  Printf.printf "Game status = %s\n" (Buffer.contents buf)
  






