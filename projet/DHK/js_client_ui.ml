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

  
  
