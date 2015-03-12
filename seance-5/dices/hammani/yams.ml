
let rec make_list size init = 
	match size with 
	| 0 -> []
	| n -> init::(make_list (n-1) init )

let rec full nb_de max = 
	let l =  make_list 3 nb_de in
	let rec aux n = 
		match n with 
		| 0 -> []
		| n -> 
		let res = l@[n;n] in
			res::(aux (n-1))
	in 
	match nb_de with 
		| 0 -> []
		| n -> (aux max)@(full (nb_de-1) max) 


let () = 
	let l = full 6 6 in
		List.iter (fun ll -> 
			List.iter (fun x -> Printf.printf "%d" x ) ll; Printf.printf "\n")  l
