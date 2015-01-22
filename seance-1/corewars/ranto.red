;author Ranto

loop:	add	#4, @3
mov	$2, @-2
jmp	loop
dat	#2, #4

end	loop