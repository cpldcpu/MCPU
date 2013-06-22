
; Primetester
;
;
;
; 27.01.01 Tim Boescke

USE "cpu3.inc"

;	sub=-2;	
;	while ((number+sub)>0)
;	{
;		test=number;
;		while (test>0) test+=sub;
;		if (test==0) return 0;
;		sub+=-1;
;	}
;	return 1;

start:

	NOR	allone
	ADD	allone
	ADD	allone
	STA	sub	;sub=-2

loop:
	NOR	allone
	ADD	number
innerloop:
	ADD	sub	;
	JCC	out
	JCC 	innerloop
out:
	NOR	zero
	ADD	one
	ADD	sub

	ADD	allone
	JCC	noprime

	NOR 	allone
	ADD	sub
	ADD	allone
	STA	sub	;sub-=1	
			
	ADD	allone
	ADD	number
	JCC	prime
	JCC	loop

prime:
	NOR	allone
	ADD	number	;load prime number

			; Add displaycode here
			; (Or breakpoint)
	JCC	next

noprime:
next:
	NOR	allone
	ADD	number
	ADD	two
	STA	number

	JCC	start

two:
	DCB	(2)
sub:
	DCB	(0)

number:
	DCB	(3)
