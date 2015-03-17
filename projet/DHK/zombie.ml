open Program

let zombie =  
	let see = Id "see" in
	let see_ant = Id "see_ant" in
	let no_ant = Binop(Sub,see_ant,Int 1) in
	let ant = Binop(Sub,see_ant,Int 2) in
	let zant = Binop(Sub,see_ant,Int 3) in
	let dead_ant = Binop(Sub,see_ant,Int 4) in 
	let grass = Binop(Sub,see,Int 2) in 
	let food = Binop(Sub,see,Int 3) in

	[Store see_ant SeeAnt;
	mk_if ant 
		[Store see See;
		mk_if grass
			[mk_if food
				[TurnRight]
				[Move]
			]
			[Move]
		]
		[Command.Attack 10] 
	]