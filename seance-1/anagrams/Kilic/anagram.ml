let create w = Hashtbl.create (String.length w);;
  
let hash_from_words table w (o : int -> int -> int) =
  let add_characters table c =
    try
      let v = Hashtbl.find table c in
      Hashtbl.replace table c (o v 1)
    with Not_found -> Hashtbl.add table c 1 in
  String.iter (fun c -> add_characters table c) w;
  table;;               

let read_line (i : in_channel) : string option =
  try Some (input_line i) with End_of_file -> None 

let rec words_to_list i = match read_line i with
  | None -> []
  | Some s -> s :: words_to_list i
                
let make_words_list src =
  let i = open_in src in
  words_to_list i 

let is_anagram w1 w2 = 
  if String.length w1 != String.length w2 then false
  else 
    let table = create w1 in
    let table = hash_from_words table w1 (+) in
    let table = hash_from_words table w2 (-) in
    Hashtbl.fold (fun _ v acc -> acc && v = 0) table true
      
let all_anagrams l =
  List.iter (fun w ->
      Printf.printf "%s : " w;
      List.iter (fun w' ->
          if not(w = w') && is_anagram w w' then
            Printf.printf "%s " w'
          else ()) l;
      Printf.printf "\n"
    ) l 

let () =
  let words = make_words_list "words" in
  all_anagrams words
  
