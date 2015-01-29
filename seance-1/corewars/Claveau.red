;redcode-94
;name protected dwarven
;author A. Claveau
;assert 1

			
					
start	MOV dwarfD, -10
	MOV dwarfD+1, -10
	MOV dwarfD+2, -10
	MOV dwarfD+3, -10
	MOV dwarfD+4, -10
	MOV dwarfD+5, -10
	MOV bomb, -10
	SPL @-7
	MOV dwarfD, -30
	MOV dwarfD+1, -30
	MOV dwarfD+2, -30
	MOV dwarfD+3, -30
	MOV dwarfD+4, -30
	MOV dwarfD+5, -30
	MOV bomb, -30
	SPL @-7
	SPL dwarfD
	JMP copie
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0		
	mov #0,0	
			
dwarfD	MOV bomb, -1
	MOV bomb, -2
	MOV bomb, -3
	MOV bomb, -4
	MOV bomb, -5
	JMP -5
		
dwarfA	ADD #4, 1
	MOV bomb, 0
	JMP -2
bomb    DAT 0, 0
		
copie	MOV dwarfA, 30
	MOV dwarfA+1, 30
	MOV dwarfA+2, 30
	MOV dwarfA+3, 30
	SPL @-4
	ADD #300, -5
	ADD #300, -5
	ADD #300, -5
	ADD #300, -5
	ADD #300, -5
	JMP copie

	end
