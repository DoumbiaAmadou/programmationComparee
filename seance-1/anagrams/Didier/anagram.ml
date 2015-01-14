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

let rec anamap m l = 
  let adm e1 e2 m = 
    if is_anagram e1 e2
    then let m = try (SMap.add e2 (e1::(SMap.find e2 m)) m)
                 with | Not_found -> SMap.add e2 [e1] m
	 in try (SMap.add e1 (e2::(SMap.find e1 m)) m)
	    with | Not_found -> SMap.add e1 [e2] m 
    else m
  in
  match l with
  | [] -> m
  | h::t -> 
    let m = List.fold_left (fun m e -> adm h e m) m t
    in anamap m t
;;

let _ =
  let map = anamap SMap.empty (List.tl (Array.to_list Sys.argv))
  in List.iter (fun (s,l) -> 
                   print_endline (s ^ " :" ^ (List.fold_left (fun s m -> s^" "^m) "" l))
  ) (SMap.bindings map)
;;
