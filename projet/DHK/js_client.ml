open Utils
open Yojson.Basic.Util

module Html = Dom_html

(* TODO : CODE A FACTORISER *)
  
let auth bsubmit url lfields = 
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_post ~url:url 
          ~post_params:fields in 
      let callback () =
        match req##readyState with
        | XmlHttpRequest.DONE -> 
          let status = req##status in
          if status = 200 then
            let cookie = Js.string "Set-Cookie" in      
            let cookie = match Js.Opt.to_option req##getResponseHeader (cookie) with
              | None -> ""
              | Some s -> Js.to_string s in
            (* TODO : Pour l'instant, la récupération du cookie ne fonctionne pas *)      
            doc##cookie <- Js.string cookie;
            win##alert (req##responseText);
        | _ -> () in 
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)

let create_game bsubmit url lfields =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_get ~url:url
          ~get_params:fields in
     let callback () =
        match req##readyState with
          | XmlHttpRequest.DONE ->
            let status = req##status in
          if status = 200 then
            win##alert (req##responseText)
          | _ -> () in
     req##onreadystatechange <- Js.wrap_callback callback;
     Js._true)

let register bsubmit url lfields =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_post ~url:url 
          ~post_params:fields in 
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

let show_games div_id url =
  let div = get_element_by_id div_id in 
  let req = Http.http_get ~url:url
      ~get_params:[] in
  let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then
        let table = Js_client_ui.create_table "table-games" in
        Js_client_ui.add_entry_to_table table "Identifier" "Creator" "Teaser";
        let response = Js.to_string req##responseText in
        let json = Yojson.Basic.from_string response in
        let games = json |> member "response" |> member "games" in
              games |> to_list 
              |> List.map (fun obj ->
                  let game_desc = obj |> member "game_description" in 
                  let id = game_desc  |> member "identifier" |> to_string in
                  let creator = game_desc  |> member "creator" |> to_string in
                  let teaser = game_desc  |> member "teaser" |> to_string in
                  (id,creator,teaser))
              |> List.iter (fun (id,creator,teaser) ->
                  Js_client_ui.add_entry_to_table table id creator teaser);
              Dom.appendChild div table
    | _ -> () in
  req##onreadystatechange <- Js.wrap_callback callback

let join_game bsubmit url lfields =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_get ~url:url
          ~get_params:fields in
     let callback () =
        match req##readyState with
          | XmlHttpRequest.DONE ->
            let status = req##status in
          if status = 200 then
            win##alert (req##responseText);
          | _ -> () in
     req##onreadystatechange <- Js.wrap_callback callback;
     Js._true)

let join_game bsubmit url lfields =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_get ~url:url
          ~get_params:fields in
     let callback () =
        match req##readyState with
          | XmlHttpRequest.DONE ->
            let status = req##status in
          if status = 200 then
            win##alert (req##responseText);
          | _ -> () in
     req##onreadystatechange <- Js.wrap_callback callback;
     Js._true)

let logout url = 
  let req = Http.http_get ~url:url ~get_params:[] in
  let callback () =
    match req##readyState with
    | XmlHttpRequest.DONE ->
      let status = req##status in
      if status = 200 then        
        win##alert (req##responseText)
    | _ -> () in
  req##onreadystatechange <- Js.wrap_callback callback

let play bsubmit url lfields =
  bsubmit##onclick <- Html.handler (fun _ ->
      let fields = List.map (fun field -> field, input_value field) lfields in 
      let req = Http.http_get ~url:url
          ~get_params:fields in
     let callback () =
        match req##readyState with
          | XmlHttpRequest.DONE ->
            let status = req##status in
            if status = 200 then
              win##alert (req##responseText);
          | _ -> () in
      req##onreadystatechange <- Js.wrap_callback callback;
      Js._true)





    

