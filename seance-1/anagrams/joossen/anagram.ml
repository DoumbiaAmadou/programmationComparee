open Arg;;

module SS = Set.Make(String);;

let string2list s =
  let rec subS2L (i: int) (l: char list) : char list =
    if i < 0 then l else subS2L (i - 1) (s.[i] :: l)
  in
  subS2L (String.length s - 1) []
;;

let rec list2string = function
  | []   -> ""
  | h::t -> (Char.escaped h) ^ list2string t
;;

let compare_char c1 c2 = Char.code c1 - Char.code c2;;

let sort_word w = list2string (List.sort (compare_char) (string2list w));;

let add_word ht nw =
  let snw = sort_word nw in
  try Hashtbl.replace ht snw (SS.add nw (Hashtbl.find ht snw))
  with Not_found -> Hashtbl.add ht snw (SS.singleton nw)
;;
  
let rec add_words ht ws = SS.iter (add_word ht) ws;;

let create_dic ws =
  let ht = Hashtbl.create (SS.cardinal ws)
  in
  SS.iter (add_word ht) ws ; ht
;;

let find_anagrams_from_string word dic =
  try Hashtbl.find dic (sort_word word)
  with Not_found -> SS.empty
;;

let print_result w anagrams =
  let my_print (word: string) : unit = print_string (" " ^ word) in
  let print_set s =  match SS.is_empty s with
                         | true  -> print_endline ""
                         | false -> SS.iter my_print s; print_string ("\n")
  in
  print_string (w ^ ":");
  print_set anagrams;
;;

let find_and_print dic w = print_result w (find_anagrams_from_string w dic);;

let find_anagrams_from_list words dic = SS.iter (find_and_print dic) words;;

let file2list filename =
  let in_file = open_in filename in
  let words = ref SS.empty in
  try
    while true; do
      words := SS.add (input_line in_file) !words
    done; SS.empty
  with e -> close_in in_file; !words
;;
  
let file_dic        = ref "";;
let cnt             = ref 1;;

let rec print_list (l: string list) =
  match l with
  | [] -> ()
  | h::t -> print_endline h; print_list t
;;

let array_to_set (a: string array) = Array.fold_right SS.add a SS.empty;;
  
let main () =
  let number_args = Array.length Sys.argv
  in
  file_dic := "../words";
  find_anagrams_from_list
    (array_to_set (Array.sub Sys.argv (1) (number_args -1)))
    (create_dic (file2list !file_dic))
;;

let run =  main ();;
