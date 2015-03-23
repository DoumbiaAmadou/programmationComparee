open Utils
open Yojson.Basic.Util

module Html = Dom_html

(* TODO : CODE A FACTORISER *)

let server_url = "http://yann.regis-gianas.org/antroid/"

let api_version = "0"

let prefix_url = server_url ^ api_version               

let format_params lfields =
  List.map (fun field -> field, input_value field) lfields

let api_action bsubmit action lfields =
  let url = prefix_url ^ "/" ^ action ^ "?" in 
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = format_params lfields in
      let req = Http.http_get ~url:url ~get_params:fields in
      let callback () =
        match req##readyState with
        | XmlHttpRequest.DONE ->
          let status = req##status in
          if status = 200 then
            win##alert (req##responseText)
        | _ -> () in
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)
  
let auth bsubmit lfields =
  let url = prefix_url ^ "/auth" in 
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = format_params lfields in
      let req = Http.http_post ~url:url ~post_params:fields in 
      let callback () =
        match req##readyState with
        | XmlHttpRequest.DONE -> 
          let status = req##status in
          if status = 200 then
            let cookie_prop = Js.string "Set-Cookie" in      
            let cookie =
              match Js.Opt.to_option req##getResponseHeader (cookie_prop) with
              | None -> ""
              | Some s -> Js.to_string s in
            (* TODO : Pour l'instant, la récupération du cookie ne fonctionne 
               pas *)
            Printf.printf "cookie = %s\n" cookie;
            doc##cookie <- Js.string cookie;
            win##alert (req##responseText);
        | _ -> () in 
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)

let create_game bsubmit lfields = api_action bsubmit "create" lfields

let register bsubmit lfields =
  let url = prefix_url ^ "/register" in 
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = format_params lfields in 
      let req = Http.http_post ~url:url ~post_params:fields in 
      let callback () =
        match req##readyState with
        | XmlHttpRequest.DONE -> 
          let status = req##status in
          if status = 200 then
            begin              
              win##alert (req##responseText); end
        | _ -> () in 
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)

let all_games response =
  let json = Yojson.Basic.from_string response in
  let games = json |> member "response" |> member "games" in
  games
  |> to_list 
  |> List.map (fun obj ->
      let game_desc = obj |> member "game_description" in 
      let id = game_desc  |> member "identifier" |> to_string in
      let creator = game_desc  |> member "creator" |> to_string in
      let teaser = game_desc  |> member "teaser" |> to_string in
      (id,creator,teaser))


let show_games div_id =
  let url = prefix_url ^ "/games" in 
  let div = get_element_by_id div_id in 
  let req = Http.http_get ~url:url ~get_params:[] in
  let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then
        let table = Js_client_ui.create_table "table-games" in
        Js_client_ui.add_entry_to_table table "Identifier" "Creator" "Teaser";
        all_games (Js.to_string req##responseText)
        |> List.iter (fun (id,creator,teaser) ->
            Js_client_ui.add_entry_to_table table id creator teaser);
        Dom.appendChild div table
    | _ -> () in
  req##onreadystatechange <- Js.wrap_callback callback

      
let destroy_game bsubmit lfields = api_action bsubmit "destroy " lfields
    
let join_game bsubmit lfields = api_action bsubmit "join" lfields

let logout () =
  let url = prefix_url ^ "/logout" in 
  let req = Http.http_get ~url:url ~get_params:[] in
  let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then        
        win##alert (req##responseText)
    | _ -> () in
  req##onreadystatechange <- Js.wrap_callback callback


let play bsubmit lfields = api_action bsubmit "play" lfields






    

