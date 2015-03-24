open Utils
open Yojson.Basic.Util

module Html = Dom_html

(* TODO : CODE A FACTORISER *)

let server_url = "http://yann.regis-gianas.org/antroid/"

let api_version = "0"

let prefix_url = server_url ^ api_version               

let input_values lfields =
  List.map (fun field -> field, input_value field) lfields

let api_action meth bsubmit action lfields callback =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = input_values lfields in      
      let req = match meth with
        | `GET ->
          let url = prefix_url ^ "/" ^ action ^ "?" in 
          Http.http_get ~url:url ~get_params:fields 
        | `POST ->
          let url = prefix_url ^ "/" ^ action in 
          Http.http_post ~url:url ~post_params:fields in
      let callback () = callback req in 
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)

let alert_with_response req =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then
        win##alert (req##responseText)
    | _ -> () 

let create_game bsubmit lfields =
  api_action `GET bsubmit "create" lfields alert_with_response 
    
let destroy_game bsubmit lfields =
  api_action `GET bsubmit "destroy " lfields alert_with_response 
    
let join_game bsubmit lfields =
  api_action `GET bsubmit "join" lfields alert_with_response 

let play bsubmit lfields =
  api_action `GET bsubmit "play" lfields alert_with_response

let register bsubmit lfields =
  api_action `POST bsubmit "register" lfields alert_with_response

let status_callback div_id req =
  match req##readyState with
  | XmlHttpRequest.DONE ->
    let status = req##status in
    if status = 200 then
      let response = Js.to_string req##responseText in
        let values = IO.game_status response in 
      Js_client_ui.show_game_status div_id values 
  | _ -> () 

let status bsubmit lfields div_id =
  api_action `GET bsubmit "status" lfields (status_callback div_id)

let auth_callback req =
  match req##readyState with
  | XmlHttpRequest.DONE -> 
    let status = req##status in
    if status = 200 then
      win##alert (req##responseText);
    Printf.printf "headers = %s\n"
      (Js.to_string req##getAllResponseHeaders ()); 
    let cookie_prop = Js.string "Set-Cookie" in      
    let cookie =
      match Js.Opt.to_option req##getResponseHeader (cookie_prop) with
      | None -> ""
            | Some s -> Js.to_string s in
          (* TODO : Pour l'instant, la récupération du cookie ne fonctionne 
               pas *)
          Printf.printf "cookie = %s\n" cookie;
          doc##cookie <-
            Js.string "fobb2dNmOLuNb0ZE+j+/BlvZoCo41d544256e18d389";
    | _ -> () 

let auth bsubmit lfields = api_action `POST bsubmit "auth" lfields auth_callback

let show_games_callback req div_id =
  let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then
        let div = get_element_by_id div_id in
        let table = Js_client_ui.create_table "table-games" in
        Js_client_ui.add_entry_to_table table "Identifier" "Creator" "Teaser";
        IO.all_games_informations (Js.to_string req##responseText)
        |> List.iter (fun (id,creator,teaser) ->
            Js_client_ui.add_entry_to_table table id creator teaser);
        Dom.appendChild div table
    | _ -> () in
  callback

let show_games div_id = 
  let url = prefix_url ^ "/games" in
  let req = Http.http_get ~url:url ~get_params:[] in
  let callback = show_games_callback req div_id in
  req##onreadystatechange <- Js.wrap_callback callback

let logout_callback req =
    let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then
        win##alert (req##responseText)
    | _ -> () in
    callback
  
let logout () =
  let url = prefix_url ^ "/logout" in
  let req = Http.http_get ~url:url ~get_params:[] in
  let callback = logout_callback req in
  req##onreadystatechange <- Js.wrap_callback callback











    

