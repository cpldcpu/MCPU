
; GCD for TI0802 using Dijkstras Algorithm
;
; Optimized for speed
;
; 12.01.01 Tim Boescke

USE "cpu3.inc"

start:
	NOR	allone
	NOR	b
	ADD	one
	STA	bneg	; bneg=-b

;----------- INNERLOOP --------------
loop:
	NOR	allone	;Akku = 0
	ADD	bneg
cont:
	ADD	a	;Akku = a - b

	JCC	neg	;jump when Akku<0

	STA	a

	ADD	allone
	JCC	end	;A=0 ? -> end,  result in b
		
	JCC	loop	;uncond.
neg:
	STA	bneg
	JCC	cont	;uncond.

;------------------------------------

end:
			;Akku contains $FF here
	ADD	bneg	 
	NOR	zero
	STA	b	; b=-bneg
wait:
	JCC	wait
	JCC	wait
a:
	DCB	(6)
b:
	DCB	(4)
bneg:	
	DCB	(0)