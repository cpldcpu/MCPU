
; GCD using Dijkstras Algorithm
;
; 12.01.01 Tim Boescke

USE "cpu3.inc"

start:
	NOR	allone	;Akku = 0
	NOR	b
	ADD	one	;Akku = - b

	ADD	a	;Akku = a - b
			;Carry set when akku >= 0
	JCC	neg

	STA	a

	ADD	allone
	JCC	end	;A=0 ? -> end,  result in b

	JCC	start
neg:
	NOR	zero
	ADD	one	;Akku = -Akku

	STA	b
	JCC	start	;Carry was not altered
end:
	JCC	end
a:
	DCB	(126)
b:
	DCB	(124)
bneg:	
	DCB	(0)