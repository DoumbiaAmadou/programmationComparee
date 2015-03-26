open Yojson.Basic.Util

(** [get_member json m] return the member [m] from [json]. *)
let get_member json m = json |> member m |> to_string
                        
let is_success response =
  let json = Yojson.Basic.from_string response in
  get_member json "status" = "completed"

let game_status response =
  let json = Yojson.Basic.from_string response in
  let game = json |> member "response" |> member "status" in
  let players =
    game
    |> member "players"
    |> to_list
    |> filter_string
    |> String.concat " " in 
  let game_member = get_member game in
  [("creator", game_member "creator");
   ("creation_date", game_member "creation_date");
   ("teaser", game_member "teaser");
   ("visibility", game_member "visibility");
   ("nb_ant_per_player", game_member "nb_ant_per_player");
   ("pace", game_member "pace");   
   ("initial_energy", game_member "initial_energy");
   ("initial_acid", game_member "initial_acid");
   ("players", players);
   ("turn", game_member "turn")]
  

let all_games_informations response =
  let json = Yojson.Basic.from_string response in
  let games = json |> member "response" |> member "games" in
  games
  |> to_list
  |> List.map (fun obj ->
      let game_desc = obj |> member "game_description" in
      let id = get_member game_desc "identifier" in 
      let creator = get_member game_desc "creator"  in 
      let teaser = get_member  game_desc "teaser" in
      (id,creator,teaser))
    

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

(* let find_cell (x,y) r = if (x,y) = (r.cx, r.cy) then Some r.content else None *)

(* let ant_loc (x,y) r = if (x,y) = (r.lx, r.ly) then Some r.lbrain else None *)
    
(* let to_ant_obs : Yojson.Basic.json -> ant_obs list = fun json -> [] *)
  
(* let orientation x y dx dy position = *)
(*   let l = [ (0,1); *)
(*             (1,1); *)
(*             (1,0); *)
(*             (1,-1); *)
(*             (0,-1); *)
(*             (-1,-1); *)
(*             (-1,0); *)
(*             (-1,1) ] in *)
  
(*   let index l v =  *)
(*     let rec index_aux l v i = match l with  *)
(*       | [] -> (-1) *)
(*       | x :: tl -> if x = v then i else index_aux tl v (i+1) in *)
(*     index_aux l v 0 in *)
  
(*   let get_orientation ind n = List.nth l (ind + n mod 8) in *)
  
(*   let aux orient = *)
(*     let ind = index l orient in *)
(*     let open Data in *)
(*     function *)
(*     | Front -> orient *)
(*     | Back -> get_orientation ind 4  *)
(*     | Left -> get_orientation ind (-2) *)
(*     | Right -> get_orientation ind (2) *)
(*     | FrontLeft -> get_orientation ind (-1) *)
(*     | FrontRight -> get_orientation ind (1) *)
(*     | BackRight -> get_orientation ind 3 *)
(*     | BackLeft -> get_orientation ind (-3) *)
(*     | On -> 0,0 in *)

(*   let (dx, dy) = aux (dx,dy) position in *)
(*   dx+x, dy+y *)
            

(* let max_state cookie = (0,0) *)

(* let rec search_data l f = match l with *)
(*   | [] -> None *)
(*   | x  :: tail -> match f x with *)
(*     | Some s as s' -> s' *)
(*     | None -> search_data tail f                 *)

(* let env ant_obs = *)
(*   let open Data in  *)
(*   let positions = [FrontLeft;Front; FrontRight; *)
(*                    Left; On; Right; *)
(*                    BackLeft; Back; BackRight] in *)
  
(*   let env_item lcell lant position = *)
(*     match search_data lcell (find_cell position), *)
(*           search_data lant (ant_loc position) with *)
(*     | None, _ -> assert false  *)
(*     | Some c, None -> (Data.field_of_string c, None) *)
(*     | Some c, Some lant -> (Data.field_of_string c, *)
(*                             Some (Data.ant_of_string lant)) in *)

(*   List.map (fun p -> *)
(*       let pos = orientation ant_obs.x ant_obs.y ant_obs.dx ant_obs.dy p in  *)
(*       env_item ant_obs.field ant_obs.ant_loc pos) positions *)
  
(* let data_from_json (menergy, macid) json =  *)
(*   let ant_obs_l = to_ant_obs json in *)
(*   List.map (fun r -> *)
(*       let state : Data.state = { *)
(*         Data.energy = r.energy; *)
(*         Data.acid = r.acid; *)
(*         Data.max_energy = menergy; *)
(*         Data.max_acid = macid *)
(*       } in *)
(*       r.id, Data.Ally (state, env r)) ant_obs_l *)

    






