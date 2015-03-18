open Program

let zombie =  
	(* let no_ant = Binop(Sub,SeeAnt,Int 1) in *)
	let ant = Binop(Sub,SeeAnt,Int 2) in
	(* let zant = Binop(Sub,SeeAnt,Int 3) in *)
	(* let dead_ant = Binop(Sub,SeeAnt,Int 4) in  *)
	let grass = Binop(Sub,See,Int 2) in 
	let food = Binop(Sub,See,Int 3) in
	
	let if_food = mk_if food 
	  [(None,Command(Data.TurnRight))] 
	  [(None,Command(Data.Move))] in
	let if_grass = mk_if grass if_food [(None,Command(Data.Move))] in
	let if_ant = mk_if ant if_grass	[(None,Command(Data.Attack 10))] in 
	if_ant
	
