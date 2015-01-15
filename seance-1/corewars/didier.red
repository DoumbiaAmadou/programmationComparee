;redcode-94
;name CowardBomber
;author K. Didier

	org start

bomb:	DAT #0,#-3
start:	SLT #5,bomb
	MOV 0,1
	MOV bomb,@bomb
	ADD #-3,bomb
	JMP start
	
	end