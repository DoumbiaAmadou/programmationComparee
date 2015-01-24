let rec add a  l = match l with 
  [] -> [(a , 1)] ; 
|(c , nb)::q -> if(c < a ) then (c, nb)::( add a q) 
else if(c= a) then  (c,nb+1)::q  else (a  , 1) :: l ;; 

let rec  string_to_couple  s le  = if((String.length  s) =0 ) then le else  string_to_couple (String.sub s 1 (String.length s -1) ) (add (String.get s 0) le )  ;;
string_to_couple "amadou" [];;

let rec occ l e  = match l with  
  [] -> 0  ;
|(c , nb)::q -> if(c=e) then  nb else occ q e;; 
 	 
let rec  contains l1 l2  =
 match l1 with 
    []->  true; 
  |(c,i)::q -> if(i = (occ l2 c )) then contains q l2 else false ;;

let read in_stream = 
try 
  Some(input_line in_stream) ; 				
with 
  End_of_file -> None ;;

let rec  read_file in_stream   list = match read in_stream with 
|None -> list ; 
|Some(a) ->  read_file in_stream (a::list) ;;
 
let rec makeAnagram s l = match  l with 
[] -> []
|t::q -> if(String.length s = String.length t && contains (string_to_couple s []) (string_to_couple t [] ) )  then t::(makeAnagram s q )else(makeAnagram s q);;

let rec  genAna l  l1 = match l with 
[] -> [] 
|t::q -> ( t , makeAnagram t l1 )::(genAna q l1) ;;
      
let () =
  if Array.length Sys.argv > 1 then

    let l = List.tl (Array.to_list Sys.argv) in
    let l = List.map String.lowercase l in 
    let in_stream = open_in "../words" in  
    let dic = read_file in_stream [] in 
    close_in in_stream ;
    List.iter  (fun (y,x) -> print_string (String.concat " " (List.map (fun r ->r ) x)^"\n"))  (genAna  l dic)  ; 
  else
    Printf.printf "usage { ./progname w1 w2 ... wn" ;; 


