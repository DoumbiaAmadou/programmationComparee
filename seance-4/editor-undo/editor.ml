type editor = text -> command -> text

and command =
  | MoveFocus of int
  | Change of char
  | Insert of char
  | Delete
  | Undo

and text = {
  size : int
  characters : char IntMap.t
}

let interpret : command -> text list -> text list =
  fun c texts ->
    match (c, texts) with
      | Undo, _ :: ts -> ts

let input_command () =
  failwith "TODO"

let rec loop texts =
  let command = input_command () in
  loop (interpret command texts)
