open Utils

module Html = Dom_html

let create_form div_id fields =
  let div = get_element_by_id "content" in
  List.iter (fun field ->
      let label = Html.createLabel doc in
      label##textContent <- Js.some (Js.string field);
      let button = Html.createInput ~_type:(Js.string "text") doc in
      button##id <- Js.string field;
      Dom.appendChild div label;
      Dom.appendChild div (Html.createBr doc);
      Dom.appendChild div button;
      Dom.appendChild div (Html.createBr doc)
    ) fields;
  let submit = Html.createInput ~_type:(Js.string "button") doc in
  submit##value <- Js.string "Submit";
  Dom.appendChild div submit;
  submit 

let create_table id =
  let table = Html.createTable doc in
  table##id <- Js.string id;
  table

let add_entry_to_table table id creator teaser =
  let tr = Html.createTr doc in
  let tid = Html.createTd doc in
  tid##textContent <- Js.some (Js.string id);
  let tcreator = Html.createTd doc in
  tcreator##textContent <- Js.some (Js.string creator);
  let tdteaser = Html.createTd doc in
  tdteaser##textContent <- Js.some (Js.string teaser);
  Dom.appendChild tr tid;
  Dom.appendChild tr tcreator;
  Dom.appendChild tr tdteaser;
  Dom.appendChild table tr

let show_games_identifiers div_id values =
  let div = get_element_by_id div_id in
  List.iter (fun (id,creator,_) ->
      let value = "Game = " ^ id ^ " created by " ^ creator in 
      let input = Utils.create_input id value in
      Dom.appendChild div input) values

let show_game_status div_id values =
  let div = get_element_by_id div_id in 
  let table = create_table "game-status" in
  List.iter(fun (attr,value) ->
      let tr = Html.createTr doc in
      let td_attr = Html.createTd doc in
      td_attr##textContent <- Js.some (Js.string attr);
      let td_value = Html.createTd doc in
      td_value##textContent <- Js.some (Js.string attr);
      Dom.appendChild tr td_attr;
      Dom.appendChild tr td_value;
      Dom.appendChild table tr;
    ) values;
  Dom.appendChild div table
  
