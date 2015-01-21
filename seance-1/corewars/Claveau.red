;redcode-94
;name protected dwarven
;author A. Claveau

	org start

start:		SPL safe
			JMP copy
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			
safe:		SEQ.I -200, -2
			JMP transfert
			SEQ.I -199, -3
			JMP transfert
			SEQ.I -198, -4
			JMP transfert
			SEQ.I -197, -5
			JMP transfert
			JMP safe
			
transfert:	MOV dwarf, -300
			MOV dwarf+1, -300
			MOV dwarf+2, -300
			MOV dwarf+3, -300
			MOV dwarf+4, -300
			MOV dwarf+5, -300
			MOV bomb, -306
			MOV copy, -307
			MOV copy+1, -307
			MOV copy+2, -307
			MOV copy+3, -307
			MOV copy+4, -307
			MOV copy+5, -307
			MOV copy+6, -307
			MOV copy+7, -307
			MOV copy+8, -307
			MOV copy+9, -307
			MOV copy+10, -307
			MOV copy+11, -307
			MOV copy+12, -307
			MOV copy+13, -307
			MOV copy+14, -307
			MOV copy+15, -307
			SPL @-23
			
			end
		
dwarf:  	ADD #4, 2
			SUB #4, 2
			MOV bomb, 4
			MOV bomb, 4
			JMP -4
bomb:    	DAT 0, 0

copy:		ADD #800, 7
			ADD #800, 7
			ADD #800, 7
			ADD #800, 7
			ADD #800, 7
			ADD #800, 7
			ADD #800, 7
			MOV dwarf, 8
			MOV dwarf+1, 8
			MOV dwarf+2, 8
			MOV dwarf+3, 8
			MOV dwarf+4, 8
			MOV dwarf+5, 8
			SPL @-6
			JMP copy

			end
