;redcode-94
;author Nguyen Nga
;name Dwarf
;strategy Bomb the core at different points
;ref: https://www.hackthissite.org/articles/read/986
;assert CORESIZE % 4 == 0

loop:	add	#4, @3
mov	$2, @2
jmp	loop
dat	#0, #0

end	loop
