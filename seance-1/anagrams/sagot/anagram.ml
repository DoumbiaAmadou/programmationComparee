(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-fra.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let sorted s = explode s |> List.sort compare |> implode

let mk_dic filename acc =
  let chan = open_in filename in
  let rec read () = match input_line chan with
    | word -> Hashtbl.add acc (sorted word) word ;
	      read ()
    | exception End_of_file -> close_in chan ; acc
  in read ()

let print_dic dic =
  Hashtbl.iter (fun hash word -> print_endline (hash ^ " -> " ^ word)) dic

let _ =
  let dic = Hashtbl.create 42 in
  let dic = mk_dic "../words" dic in
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.iter
       (fun word ->
	print_string word ;
	print_string " : " ;
	print_endline (String.concat " " (Hashtbl.find_all dic (sorted word)
					  |> List.sort compare)))
