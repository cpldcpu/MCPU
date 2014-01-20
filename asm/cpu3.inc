; TI0802 Include file for SMAL32
;
; 12.01.2001 Tim B"oscke
;


MACRO checkmem =mem
	IF ( mem<0 ) ! ( mem>63 )
		ERROR Adress out of range!
	ENDIF
ENDMAC

;------------------------------
;Instructions

;Branch if carry clear and clear carry

MACRO JCC tgt
	checkmem tgt
	B ( 2#11000000 ! tgt )
ENDMAC

;Add (src) to Akku

MACRO ADD src
	checkmem src
	B ( 2#01000000 ! src )
ENDMAC

;Nor Akku = Akku NOR (src)

MACRO NOR src
	checkmem src
	B ( 2#00000000 ! src )
ENDMAC

;STA (SRC) = Akku

MACRO STA tgt
	checkmem tgt
	B ( 2#10000000 ! tgt )
ENDMAC

;------------------------------
;some constants
;

.=61
zero:
	B 0
allone:
	B 255
one:
	B 1
.=0

;------------------------------
;Useful Macros

MACRO MOVE src,tgt
	NOR allone
	ADD src
	STA tgt
ENDMAC

MACRO CLR
	NOR allone
ENDMAC


MACRO JMP tgt
	JCC tgt
	JCC tgt
ENDMAC

MACRO JCS tgt
	JCC .+2
	JCC tgt
ENDMAC

MACRO NOTMOVE tgt,src
	NOR allone
	NOR src
	STA tgt
ENDMAC

MACRO LDA src
	NOR allone
	ADD src
ENDMAC

MACRO NOT 
	NOR zero
ENDMAC

; Akku = (mem) - Akku
; carry = 1 if result = 0 

MACRO SUB mem
	NOR zero
	ADD mem
	ADD one
ENDMAC

MACRO DCB (xx)
	B xx
ENDMAC
