open Data
open AI 
open Program
open IO
open Utils
open Http
open Js_client
open Js_client_ui

module Html = Dom_html

(* type army = (int * ant) list *)
(* type ia_army=  (int * Test.t) list *)
(* (\* Appel l'IA pour toute les fourmis et  *)
(* renvoi le string de l'ensemble des commandes de toutes les fourmis *\) *)
(* let rec turn ia_army army (cmd,ias) = *)
(*   match army with *)
(*   | [] -> (cmd,ias) *)
(*   |(id,a)::t ->let (c,aut) = Test.step a (List.assoc id ia_army) in *)
(*    turn ia_army t (id^":"^(string_of_command c)^cmd,(id,aut)::ias) *)

(* (\* Juste pour le permier tour*\) *)
(* let begi id_game nb_ant cookie= *)
(*   let rec fst_turn i = *)
(*     match i with *)
(*     | 0 -> "" *)
(*     | n -> (string_of_int (n-1))^":"^(string_of_command (Rest))^"," *)
(*        ^(aux (i-1)) *)
(*   in *)
(*   play cookie id_game (fst_turn nb_ant) *)

(* (\* Boucle de jeu *\) *)
(* let game id_game nb_ant cookie = *)
(*   let army = begi id_game nb_ant cookie in *)
(*   let rec aux ia_army army= *)
(*     match army with *)
(*     | [] -> () *)
(*     | l -> let (cmd,ia_army) = turn ia_army army ("",[]) *)
(* 	   in aux ia_army (play cookie id_game cmd) *)
(*   in aux (List.mapi (fun i a -> (i,Test.start)) army) army *)
	  
(** [set_handler div_id id handler] attach the [handler] function to the 
    the button having the identifier [id]. Before, the div [div_id] is cleared.
*)
let set_handler div_id id handler =
  let button = get_element_by_id id in
  button##onclick <- Html.handler (fun _ ->
      Utils.clear_div div_id;
      handler ();
      Js._true)

(** [auth_handler div_id fields] create a form which allow to the user to 
    make the authentification. *)
let auth_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form div_id fields in
  Js_client.auth submit fields

(** [game_creation_handler div_id fields] create a form in order to create a 
    new game *)
let game_creation_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form div_id fields in
  Js_client.create_game submit fields

(** [show_games_handler div_id] show the informations of every created games. *)
let show_games_handler div_id = fun () -> Js_client.show_games div_id

(** [destroy_handler div_id fields] create a form in order to destroy a game
     using his identifier. *)
let destroy_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form  div_id fields in
  Js_client.destroy_game submit fields

(** [join_handler div_id fields] create a form in order to join a game
     using his identifier. *)
let join_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form div_id fields in
  Js_client.join_game submit fields

(** [logout_handler] logout a user *)
let logout_handler = fun () -> Js_client.logout ()

(** [play_handler div_id fields] allow to a user to play a game. *)
let play_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form  div_id fields in
  Js_client.play submit fields

(** [register_handler div_id fields] create a form in order to register a new
    user *)
let register_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form div_id fields in
  Js_client.register submit fields

(** [status_handler div_id fields] show the informations about a game which the
    id is given by the user in the form. *)
let status_handler div_id fields = fun () -> 
  let submit = Js_client_ui.create_form div_id fields in
  Js_client.status submit fields div_id 

let () =
  let id_field = ["id"] in 
  let div_content_id = "content" in

  (** We add to every button in the page his corresponding handler *)

  set_handler div_content_id "auth"
    (auth_handler div_content_id ["login";"password"]);

  let create_games_fields =
    ["users"; "teaser"; "pace"; "nb_turn";
     "nb_ant_per_player"; "nb_player";
     "minimal_nb_player"; "initial_energy"; "initial_acid"] in
  
  set_handler div_content_id "create"
    (game_creation_handler div_content_id create_games_fields);

  set_handler div_content_id "destroy"
    (destroy_handler div_content_id id_field);

    set_handler div_content_id "games"
      (show_games_handler div_content_id);
  
  set_handler div_content_id "join"
    (join_handler div_content_id id_field);

  set_handler div_content_id "logout"
    (logout_handler);

  set_handler div_content_id "play"
    (play_handler div_content_id (id_field @ ["cmds"]));

  set_handler div_content_id "register"
    (register_handler div_content_id ["login";"password"]);

  set_handler div_content_id "status"
    (status_handler div_content_id id_field)

    













  
  
