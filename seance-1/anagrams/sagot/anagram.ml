let hash s =
  let len = String.length s in
  let a = Array.make len ' ' in
  let b = Bytes.create len in
  String.iteri (fun i c -> Array.set a i c) s ;
  Array.sort compare a ;
  Array.iteri (fun i c -> Bytes.set b i c) a ;
  Bytes.to_string b

let dictionnary f =
  let d = Hashtbl.create 42 in
  let chan = open_in f in
  (try while true do
	 let word = input_line chan in
	 Hashtbl.add d (hash word) word
       done
   with End_of_file -> close_in chan) ;
  d

let anagrams d w = Hashtbl.find_all d (hash w)

let _ =
  let d = dictionnary "../words" in
  let print_a w =
    Printf.printf "%s: %s\n" w (anagrams d w |> String.concat " ")
  in
  Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
  |> Array.iter print_a
