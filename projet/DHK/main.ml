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

(* let rec turn ia_army army (cmd,ias) = *)
(*   match army with *)
(*   | [] -> (cmd,ias) *)
(*   |(id,a)::t ->let (c,aut) = Test.step a (List.assoc id ia_army) in *)
(*    turn ia_army t (id^":"^(string_of_command c)^cmd,(id,aut)::ias) *)

(* let begi id_game nb_ant cookie=  *)
(*   let rec fst_turn i =  *)
(*     match i with  *)
(*     | 0 -> "" *)
(*     | n -> (string_of_int (n-1))^":"^(string_of_command (Rest))^"," *)
(*        ^(aux (i-1)) *)
(*   in  *)
(*   play cookie id_game (fst_turn nb_ant) *)

(* let game id_game nb_ant cookie = *)
(*   let army = begi id_game nb_ant cookie in  *)
(*   let rec aux ia_army army= *)
(*     match army with  *)
(*     | [] -> () *)
(*     | l -> let (cmd,ia_army) = turn ia_army army ("",[]) *)
(* 	   in aux ia_army (play cookie id_game cmd) *)
(*   in aux (List.mapi (fun i a -> (i,Test.start)) army) army *)
	  
(* let doc = Html.document *)
(* let win = Html.window *)
(* let window = Html.window *)

  
(* let get_element_by_id id = *)
(*   Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false) *)

(* let make_request meth url asyn msg = *)
(*   let req = XmlHttpRequest.create () in *)
(*   req##_open(Js.string meth, Js.string url, Js.bool asyn); *)
(*   let f () = *)
(*     let cookie = Js.string "Set-Cookie" in  *)
(*     match req##readyState with *)
(*     | XmlHttpRequest.DONE -> *)
(*       let i = req##status in *)
(*       if i = 200 then *)
(*         begin *)
(*           Printf.printf "%s\n" (Js.to_string req##getAllResponseHeaders ()); *)
(*           match Js.Opt.to_option (req##getResponseHeader (cookie)) with *)
(*           | None -> Printf.printf "Empty cookie\n" *)
(*           | Some s -> Printf.printf "Cookie = %s\n" (Js.to_string s) end; *)
(*       Printf.printf "response = %s\n" (Js.to_string (req##responseText)); *)
(*     | XmlHttpRequest.LOADING -> *)
(*       let i = req##status in *)
(*       if i = 200 then *)
(*         Printf.printf "response = %s\n" (Js.to_string (req##responseText)); *)
(*     | _ -> () in  *)
(*   req##onreadystatechange <- Js.wrap_callback f; *)
(*   req##setRequestHeader(Js.string "withCredentials", Js.string "true"); *)
(*   req##setRequestHeader(Js.string "Content-Type", *)
(* 			Js.string "application/x-www-form-urlencoded"); *)
(*   req##setRequestHeader(Js.string "Cookie", *)
(* 			Html.document##cookie); *)
(*   req##send(Js.some (Js.string msg)) *)


(* let create_form div_id fields = *)
(*   Printf.printf "here\n"; *)
(*   let div = get_element_by_id "content" in *)
(*   List.iter (fun field -> *)
(*       let label = Html.createLabel doc in *)
(*       label##textContent <- Js.some (Js.string field); *)
(*       let button = Html.createInput ~_type:(Js.string "text") doc in *)
(*       button##id <- Js.string field; *)
(*       Dom.appendChild div label; *)
(*       Dom.appendChild div (Html.createBr doc); *)
(*       Dom.appendChild div button; *)
(*       Dom.appendChild div (Html.createBr doc) *)
(*     ) fields; *)
(*   let submit = Html.createInput ~_type:(Js.string "button") doc in *)
(*   submit##textContent <- Js.some (Js.string "Submit"); *)
(*   submit##onclick <- Html.handler (fun _ -> *)
(*       let req = http_post ~url:"http://yann.regis-gianas.org/antroid/0/auth" *)
(*           ~post_params:[("login","afk"); ("password","dersimantep627")] in  *)
(*       let f () = *)
(*         match req##readyState with *)
(*         | XmlHttpRequest.DONE -> *)
(*           let i = req##status in *)
(*           if i = 200 then *)
(*             win##alert (req##responseText) *)
(*         | _ -> () in *)
(*       req##onreadystatechange <- Js.wrap_callback f; *)

(*       Js._true); *)
(*   Dom.appendChild div submit *)

let test_auth id fields =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      let submit = Js_client_ui.create_form "content" fields in
      Js_client.auth
        submit
        "http://yann.regis-gianas.org/antroid/0/auth"
        fields;
        Js._true))

let test_game_creation id fields =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      let submit = Js_client_ui.create_form  "content" fields in 
      Js_client.create_game
        submit
        "http://yann.regis-gianas.org/antroid/0/create?"
        fields;
      Js._true))

let test_show_games id =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      Js_client.show_games
        "content"
        "http://yann.regis-gianas.org/antroid/0/games";
      Js._true))

let test_join_game id fields =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      let submit = Js_client_ui.create_form  "content" fields in 
      Js_client.join_game
        submit
        "http://yann.regis-gianas.org/antroid/0/join?"
        ["id"];
      Js._true))

let test_logout id =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      Js_client.logout
        "http://yann.regis-gianas.org/antroid/0/logout";
      Js._true))

let test_play id fields =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      let submit = Js_client_ui.create_form  "content" fields in 
      Js_client.play 
        submit
        "http://yann.regis-gianas.org/antroid/0/play?"
        fields;
      Js._true))

let test_register id fields =
  let button = get_element_by_id id in
  button##onclick <- (Html.handler (fun _ ->
      let submit = Js_client_ui.create_form  "content" fields in 
      Js_client.register
        submit
        "http://yann.regis-gianas.org/antroid/0/register"
        fields;
      Js._true))



let () =
  test_auth "auth" ["login"; "password"];
  test_game_creation "create" ["users"; "teaser"; "pace"; "nb_turn";
                               "nb_ant_per_player"; "nb_player";
                               "minimal_nb_player"; "initial_energy";
                               "initial_acid"];
  test_show_games "games";
  test_join_game "join" ["id"];
  test_logout "logout";
  test_play "play" ["id";"cmds"];
  test_register "register" ["login";"password"]
    
  
  
