;redcode-94
;name IDK Dwarf like
;author Alain Dias
;strategy Dwarf like but moves

;assert CORESIZE > 8 == 1

	org	loop

loop:	add.ab	#8, bomb
	mov.i	bomb, @bomb
	mov	0,4
	jmp	loop
bomb:	dat	#0, #0

	end
