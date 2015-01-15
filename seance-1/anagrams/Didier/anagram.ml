module SMap = Map.Make(String)

let is_anagram s1 s2 = 
  let to_list s =
    let rec aux i l =
      if i < 0 then l else aux (i - 1) (s.[i] :: l) 
    in 
    aux (String.length s - 1) []
  in
  (List.sort compare (to_list s1)) = (List.sort compare (to_list s2))
;;

let rec anamap m l dico= 
  let adm e1 e2 m = 
    if is_anagram e1 e2
    then try (SMap.add e1 (e2::(SMap.find e1 m)) m)
	    with | Not_found -> SMap.add e1 [e2] m 
    else m
  in
  match l with
  | [] -> m
  | h::t -> 
    let n = List.fold_left (fun m e -> adm h e m) m dico
    in anamap n t dico
;;

let words = 
   let rec read f l = 
      try (read f ((input_line f)::l) )
      with End_of_file -> l
   in read (open_in "../words") []
;;

let _ =
  let map = anamap SMap.empty (List.tl (Array.to_list Sys.argv)) words in 
  List.iter (fun (s,l) -> 
                   print_endline (s ^ " :" ^ (List.fold_left (fun w m -> 
				                w^" "^m) "" l))
  ) (SMap.bindings map)
;;
