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
    
 
let rec exec l bd =
  match l with
    | []-> ()  
    | h::t -> print h;Printf.printf ": "; print_ana (anagrame h bd);Printf.printf "\n"; exec t bd
;;

let rec read_lines f = 
try(
  let l = (input_line f)
  in l::read_lines f 
)with
| End_of_file -> []
;;
 
(* Met les arguments de la ligne de commande dans une liste *)
let get_arg =
  let rec aux i = 
    match i with
    | 2 -> []
    | n -> Sys.argv.(i-1)::aux (i-1)
  in aux (Array.length Sys.argv)

let () =
  let arg = Array.length Sys.argv in
  
  if arg==1 then  Printf.printf "Il faut mettre en parametre une base de données de mots séparés par des espaces"
  else
    let fichier = open_in Sys.argv.(1) in
    let database = read_lines fichier in
    if arg == 2 then  exec ["marion";"manoir";"test";"ironique";"onirique"] database
    else     
    let param = get_arg in  
    exec param database
	   

