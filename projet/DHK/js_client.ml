open Utils
open Yojson.Basic.Util

module Html = Dom_html

(** The static server url *)
let server_url = "http://yann.regis-gianas.org/antroid/"

(** The API version *)
let api_version = "0"

(** This prefix should be not change during this project *)
let prefix_url = server_url ^ api_version               

(** [input_values inputs] return a tuple corresponding to the input identifier
    and his value. *)
let input_values inputs =
  List.map (fun input -> input, input_value input) inputs

(** [api_action meth bsubmit action params callback] send a HTTP request using
    the method [meth] and the parameters [params] when the user click on the 
    submit button [bsubmit]. Also, after to have click to the submit button,
    the [callback] function is called. *)
let api_action meth bsubmit action params callback =
  bsubmit##onclick <- Html.handler (fun _ ->
      let data = input_values params in      
      let req = match meth with
        | `GET ->
          let url = prefix_url ^ "/" ^ action ^ "?" in 
          Http.http_get ~url:url ~get_params:data
        | `POST ->
          let url = prefix_url ^ "/" ^ action in 
          Http.http_post ~url:url ~post_params:data in
      let callback () = callback req in 
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)

(** [alert_with_response req] show the content of the request [req] 
    in a window. *)
let alert_with_response req =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200
      then
        if IO.is_success (Js.to_string (req##responseText))
        then win##alert (req##responseText)
    | _ -> () 

let create_game bsubmit params =
  api_action `GET bsubmit "create" params alert_with_response 
    
let destroy_game bsubmit params =
  api_action `GET bsubmit "destroy" params alert_with_response 
    
let join_game bsubmit params =
  api_action `GET bsubmit "join" params alert_with_response 

let play bsubmit params =
  api_action `GET bsubmit "play" params alert_with_response

let register bsubmit params =
  api_action `POST bsubmit "register" params alert_with_response

let status_callback div_id req =
  match req##readyState with
  | XmlHttpRequest.DONE ->
    let status = req##status in
    if status = 200 then
      let response = Js.to_string req##responseText in
      let values = IO.game_status response in 
      Js_client_ui.show_game_status div_id values 
  | _ -> () 

let status bsubmit params div_id =
  api_action `GET bsubmit "status" params (status_callback div_id)

let auth_callback req =
  match req##readyState with
  | XmlHttpRequest.DONE -> 
    let status = req##status in
    if status = 200 then
      begin
        win##alert (req##responseText);
        let cookie_prop = Js.string "Set-Cookie" in      
        let cookie =
          match Js.Opt.to_option req##getResponseHeader (cookie_prop) with
          | None -> ""
          | Some s -> Js.to_string s in
        (* TODO : Pour l'instant, la récupération du cookie ne fonctionne 
               pas *)
        Printf.printf "cookie = %s\n" cookie;
        doc##cookie <- Js.string cookie
      end
  | _ -> () 

let auth bsubmit params = api_action `POST bsubmit "auth" params auth_callback

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











    

