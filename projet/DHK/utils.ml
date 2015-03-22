module Html = Dom_html

let doc = Html.document
let win = Html.window

let get_element_by_id id =
  Printf.printf "id = %s\n" id;
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false)


let input_value id =
  let input = get_element_by_id id in
  let input = Js.Unsafe.coerce input in 
  (* match Js.Opt.to_option (input##textContent) with *)
  (* | None -> assert false *)
  (* | Some s -> Js.to_string s  *)
  Js.to_string (input##value) 
  
