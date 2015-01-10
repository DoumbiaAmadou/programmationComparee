(* [hash_from_words table w o ] remplit la de hachage [table] avec le nombre 
   d'occurrences de chaque lettre du mot [w]. *)
let hash_from_words table w (o : int -> int -> int) =
  let add_characters table c =
    try
      let v = Hashtbl.find table c in
      Hashtbl.replace table c (o v 1)
    with Not_found -> Hashtbl.add table c 1 in
  String.iter (fun c -> add_characters table c) w;
  table;;

(* [read_line i] retourne une ligne du fichier via le channel [i] et EOF si
   on se trouve en fin de fichier. *)
let read_line (i : in_channel) : string option =
  try Some (input_line i) with End_of_file -> None 

(* [make_words_list i] retourne la liste de mots contenus 
   dans le fichier [src].*)
let make_words_list src =
  let rec words_to_list i = match read_line i with
    | None -> []
    | Some s -> s :: words_to_list i in
  let i = open_in src in
  words_to_list i 

(* [is_anagram w1 w2] vérifie si les mots [w1] et [w2] sont des anagrammes.*)
let is_anagram w1 w2 =
  if String.length w1 != String.length w2 then false
  else
    let table = Hashtbl.create (String.length w1) in 
    let table = hash_from_words table w1 (+) in
    let table = hash_from_words table w2 (-) in
    Hashtbl.fold (fun _ v acc -> acc && v = 0) table true
      
(* [anagrams l words] affiche tous les anagrammes des mots de [l] contenus
   dans la liste [words]. *)
let anagrams l words =
  List.iter (fun w ->
      Printf.printf "%s : " w;
      List.iter (fun w' ->
          if not(w = w') && is_anagram w w' then
            Printf.printf "%s " w'
          else ()) words;
      Printf.printf "\n"
    ) l

let () =
  if Array.length Sys.argv > 1 then
    let l = List.tl (Array.to_list Sys.argv) in
    let words_list = make_words_list "words" in
    anagrams l words_list
  else
    Printf.printf "Usage : ./anagram w1 w2 ... wn"
  
