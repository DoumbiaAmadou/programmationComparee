module Html = Dom_html

let doc = Html.document
let win = Html.window

let get_element_by_id id =
  Printf.printf "id = %s\n" id;
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false)

let input_value id =
  let input = get_element_by_id id in
  let input = Js.Unsafe.coerce input in 
  Js.to_string (input##value)

let clear_div div_id =
  let div = get_element_by_id div_id in
  Dom.list_of_nodeList (div##childNodes)
  |> List.iter (fun child -> Dom.removeChild div child)


