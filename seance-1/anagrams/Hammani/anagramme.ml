(* Decompose un string en char list*)
let decompose m =
  let rec dec i l =
    if i<0 then l
    else dec (i-1) (m.[i]::l)
  in  (dec ((String.length m) -1)) []
   
(* Converti un char list en String*)
let toString l =
  let s = String.create (List.length l)
  in let rec aux l i =
       match l with 
       | [] -> s
       | h::t -> s.[i]<-h ; aux t (i+1)
     in aux l 0
		 
(* Tri chaque String de la liste en un String trie par ordre alphabétique
ex: beau -> abeu *)

let rec arange s = toString (List.sort compare (decompose s))

(* Rearange chaque mot en une suite de lettre triee *)
let rec tri l =  
  match l with
  | [] -> []
  | h::t ->  (arange h)::(tri t) 

(* Renvoi la liste des anagramme de e contenu dans l*)						  
let rec anagrame e l =
  let ea = arange  e
  in match l with 
     | [] -> []
     | h::t -> if (compare ea  (arange h) )==0 && compare e h !=0
	       then h::anagrame e t
	       else anagrame e t

let print e =  Printf.printf "%s " e

let rec print_ana l = 
  match l with 
  | [] -> ()
  | h::t -> print h; print_ana t	    
    
 
let exec l =
  let rec aux l lt =
  match l with
    | []-> 0  
    | h::t -> print h;Printf.printf ": "; print_ana (anagrame h lt);Printf.printf "\n"; aux t lt
  in aux l l
;;

let rec read_lines f i = 
try(Printf.printf "%d\n" i;
  (input_line f)::read_lines f (i+1)
)with
| End_of_file -> []
;;
 
(*let fichier = open_in Sys.argv.(1);;
let l = read_lines fichier 0;;
Printf.printf "Fichier lu !";;  
  exec l;;*)
exec ["marion";"manoir";"minora";"test";"ironique";"onirique"];;


