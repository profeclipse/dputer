	.cpu "w65c02"

	.include "kernel.lbl"
	.include "forth.inc"
	.include "forth.mac"

;-----------------------------------------------------------------------------
; FORTH Dictionary Layout
;
;-----------------------------------------------------------------------------
; Header Structure:
; 
;     [ count byte ]  0 1   NFA <-+
;     [ name bytes ]              |
;     [ nfa link   ]  n+0   NLA >-+
;     [ link field ]  n+2   LFA >- previous word's NFA
;     [ code field ]  n+4   CFA
;     [ body       ]  n+6   PFA
; 
; Vocabulary Structure:
; 
;     [ count byte ]  0 1   NFA <-+
;     [ name bytes ]              |
;     [ nfa link   ]  n+0   NLA >-+
;     [ link field ]  n+2   LFA >- previous word's NFA
;     [ code field ]  n+4   CFA
;     [ voc link   ]  n+6   VLINK
;     [ voc        ]  n+8   VOC
;-----------------------------------------------------------------------------

	* = $0200

ENTRY:
	jmp ICOLD

;*****************************************************************************
; Forth NEXT/EXEC Engine
;*****************************************************************************

PUSHNEXT:
	jsr dpush
NEXT:
	lda (IP)
	sta CFA
	ldy #1
	lda (IP),y
	sta CFA+1
	jsr incIP
EXEC:
	lda (CFA)
	sta RCFA
	ldy #1
	lda (CFA),y
	sta RCFA+1
	jmp (RCFA)

;=============================================================================
; Runtime for Forth Words
;=============================================================================

DOCOLON:
	lda IP
	ldx IP+1
	jsr rpush
	clc
	lda CFA
	adc #2
	sta IP
	lda CFA+1
    bcc +
	;adc #0
    inc a
+	sta IP+1
	jmp NEXT
	
DOVAR:
	clc
	lda CFA
	adc #2
	tay
	lda CFA+1
	adc #0
	tax
	tya
	jmp PUSHNEXT

DOCON:
	ldy #3
	lda (CFA),y
	tax
	dey
	lda (CFA),y
	jmp PUSHNEXT

DO2CON:
	ldy #5
	lda (CFA),y
	tax
	dey
	lda (CFA),y
	jsr dpush
	ldy #3
	lda (CFA),y
	tax
	dey
	lda (CFA),y
	jmp PUSHNEXT

DODOES:			; we will get here via JSR
	lda IP
	ldx IP+1
	jsr rpush
	pla			; low byte of return addr
	sta IP
	pla			; high byte of return addr
	sta IP+1
	inc IP		; add 1 to DOES> portion of word
	bne +
	inc IP+1
+	jmp DOVAR

DODEFER:
	ldy #2
	lda (CFA),y
	sta GP0
	iny
	lda (CFA),y
	sta GP0+1
	lda GP0
	sta CFA
	lda GP0+1
	sta CFA+1
	jmp EXEC

DOVALUE:
	ldy #3
	lda (CFA),y
	tax
	dey
	lda (CFA),y
	jmp PUSHNEXT

DOVALUESTORE:
	lda CFA
	sta GP0
	lda CFA+1
	sta GP0+1
	sec
	lda GP0
	sbc #2
	sta GP0
	bcs +
	dec GP0+1
+   jsr dpop
	sta (GP0)
	txa
	ldy #1
	sta (GP0),y
	jmp NEXT

DOVALUEPSTORE:
	lda CFA
	sta GP0
	lda CFA+1
	sta GP0+1
	sec
	lda GP0
	sbc #4
	sta GP0
	lda GP0+1
	sbc #0
	sta GP0+1
	jsr dpop
	clc
	adc (GP0)
	sta (GP0)
	txa
	ldy #1
	adc (GP0),y
	sta (GP0),y
	jmp NEXT

DO2VALUE:
	ldy #2			; get address of value
	lda (CFA),y
	sta GP0
	iny
	lda (CFA),y
	sta GP0+1
	ldy #3			; get value
	lda (GP0),y
	tax
	dey
	lda (GP0),y
	jsr dpush
	dey
	lda (GP0),y
	tax
	dey
	lda (GP0),y
	jmp PUSHNEXT

DOVOC:
	clc
	lda CFA
	adc #4
	sta CFA
	bcc +
	inc CFA+1
+	lda CFA
	sta CONTEXT+2
	lda CFA+1
	sta CONTEXT+3
	jmp NEXT

;-----------------------------------------------------------------------------
; Forth Data Stack
;-----------------------------------------------------------------------------

incSP .proc
	clc
	lda SP
	adc #2
	sta SP
	bcc +
	inc SP+1
+	rts
	.endproc

decSP .proc
	sec
	lda SP
	sbc #2
	sta SP
	bcs +
	dec SP+1
+	rts
	.endproc

dpush .proc
	sta (SP)
	txa
	ldy #1
	sta (SP),y
	jmp decSP
	.endproc

dpop .proc
	jsr incSP
	ldy #1
	lda (SP),y
	tax
	lda (SP)
	rts
	.endproc

dpopToGP0 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP0+1
	lda (SP)
	sta GP0
	rts
	.endproc

dpopToGP1 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP1+1
	lda (SP)
	sta GP1
	rts
	.endproc

dpopToGP2 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP2+1
	lda (SP)
	sta GP2
	rts
	.endproc

dpopToGP3 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP3+1
	lda (SP)
	sta GP3
	rts
	.endproc

dpopToGP4 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP4+1
	lda (SP)
	sta GP4
	rts
	.endproc

dpopToGP5 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP5+1
	lda (SP)
	sta GP5
	rts
	.endproc

dpopToGP6 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP6+1
	lda (SP)
	sta GP6
	rts
	.endproc

dpopToGP7 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP7+1
	lda (SP)
	sta GP7
	rts
	.endproc

dpopToGP8 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP8+1
	lda (SP)
	sta GP8
	rts
	.endproc

dpopToGP9 .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta GP9+1
	lda (SP)
	sta GP9
	rts
	.endproc

dpopToCFA .proc
	jsr incSP
	ldy #1
	lda (SP),y
	sta CFA+1
	lda (SP)
	sta CFA
	rts
	.endproc

;-----------------------------------------------------------------------------
; Forth Return Stack
;-----------------------------------------------------------------------------

incRP .proc
	clc
	lda RP
	adc #2
	sta RP
	bcc +
	inc RP+1
+	rts
	.endproc

decRP .proc
	sec
	lda RP
	sbc #2
	sta RP
	bcs +
	dec RP+1
+	rts
	.endproc

rpop .proc
	jsr incRP
	ldy #1
	lda (RP),y
	tax
	lda (RP)
	rts
	.endproc

rpush .proc
	sta (RP)
	ldy #1
	txa
	sta (RP),y
	jsr decRP
	rts
	.endproc

;-----------------------------------------------------------------------------
; Low-Level File IO
;-----------------------------------------------------------------------------

;------------------------------------------------------------------------
; Wait for FileIO Ready Status
;------------------------------------------------------------------------
waitFileIO .proc
	lda FILEIO_CREADY
	bne waitFileIO
	rts
	.endproc

;------------------------------------------------------------------------
; Execute Current FileIO Request
;------------------------------------------------------------------------
doFileIO .proc
	lda #1
	sta FILEIO_CREADY
	bra waitFileIO
	.endproc

;------------------------------------------------------------------------
; Write data at addr,len to file fid
; On entry:
;	addr - GP1
;   len  - GP2
;   fid  - FILEIO_CFD
; On exit:
;	a -> status
;	x -> 0
;------------------------------------------------------------------------
doWriteFile .proc
	lda #FILEIO_CMD_WRITE
	sta FILEIO_CCMD
-	lda GP2
	ora GP2+1
	beq +++
	lda (GP1)
	sta FILEIO_CDATA_LO
	jsr doFileIO
	lda FILEIO_CSTATUS
	bne +++
	inc GP1
	bne +
	inc GP1+1
+
	sec
	lda GP2
	sbc #1
	sta GP2
	bcs -
	dec GP2+1
+	bra -
+	lda FILEIO_CSTATUS
	ldx #0
	rts
	.endproc

;*****************************************************************************
; Utility Subroutines Used throughout Forth System
;*****************************************************************************

incIP .proc
	clc
	lda IP
	adc #2
	sta IP
	bcc +
	inc IP+1
+	rts
	.endproc

incGP0 .proc
	inc GP0
	bne +
	inc GP0+1
+	rts
	.endproc

decGP0 .proc
	sec
	lda GP0
	sbc #1
	sta GP0
	bcs +
	dec GP0+1
+	rts
	.endproc

incGP1 .proc
	inc GP1
	bne +
	inc GP1+1
+	rts
	.endproc

decGP1 .proc
	sec
	lda GP1
	sbc #1
	sta GP1
	bcs +
	dec GP1+1
+	rts
	.endproc

incGP2 .proc
	inc GP2
	bne +
	inc GP2+1
+	rts
	.endproc

decGP2 .proc
	sec
	lda GP2
	sbc #1
	sta GP2
	bcs +
	dec GP2+1
+	rts
	.endproc

incGP3 .proc
	inc GP3
	bne +
	inc GP3+1
+	rts
	.endproc

decGP3 .proc
	sec
	lda GP3
	sbc #1
	sta GP3
	bcs +
	dec GP3+1
+	rts
	.endproc

incGP4 .proc
	inc GP4
	bne +
	inc GP4+1
+	rts
	.endproc

decGP4 .proc
	sec
	lda GP4
	sbc #1
	sta GP4
	bcs +
	dec GP4+1
+	rts
	.endproc

incGP5 .proc
	inc GP5
	bne +
	inc GP5+1
+	rts
	.endproc

decGP5 .proc
	sec
	lda GP5
	sbc #1
	sta GP5
	bcs +
	dec GP5+1
+	rts
	.endproc

incGP6 .proc
	inc GP6
	bne +
	inc GP6+1
+	rts
	.endproc

decGP6 .proc
	sec
	lda GP6
	sbc #1
	sta GP6
	bcs +
	dec GP6+1
+	rts
	.endproc

incGP7 .proc
	inc GP7
	bne +
	inc GP7+1
+	rts
	.endproc

decGP7 .proc
	sec
	lda GP7
	sbc #1
	sta GP7
	bcs +
	dec GP7+1
+	rts
	.endproc

incGP8 .proc
	inc GP8
	bne +
	inc GP8+1
+	rts
	.endproc

decGP8 .proc
	sec
	lda GP8
	sbc #1
	sta GP8
	bcs +
	dec GP8+1
+	rts
	.endproc

;*****************************************************************************
; And so we begin...
;*****************************************************************************

FORTHKERNEL:
	.word 0
	.ptext "forth.s"

	.varword "LOADFILE",LOADFILE
	.word FORTHKERNEL

;*****************************************************************************
; Forth Constants
;*****************************************************************************

	.constword "VERSION#",VERSIONNUMBER,VERSIONNUM
	.constword "MAXSTRING",MAXSTRING,255
	.constword "MAXCOUNTED",MAXCOUNTED,255
	.constword "MAX-NAME-CHARS",MAXNAMECHARS,63
	.constword "0",ZERO,0
	.constword "1",ONE,1
	.constword "2",TWO,2
	.constword "TRUE",TRUECON,$FFFF
	.constword "FALSE",FALSECON,0
	.constword "CELL",CELL,2
	.constword "BL",BL,32
	.constword "TOP-OF-MEM",TOPOFMEM,RSTACK

	.constword "DOCOLON",DOCOLONCON,DOCOLON
	.constword "DOVAR",DOVARCON,DOVAR
	.constword "DOCON",DOCONCON,DOCON
	.constword "DO2CON",DO2CONCON,DO2CON
	.constword "DODOES",DODOESCON,DODOES
	.constword "DODEFER",DODEFERCON,DODEFER
	.constword "DOVALUE",DOVALUECON,DOVALUE
	.constword "DOVALUE!",DOVALUESTORECON,DOVALUESTORE
	.constword "DOVALUE+!",DOVALUEPSTORECON,DOVALUEPSTORE
	.constword "DO2VALUE",DO2VALUECON,DO2VALUE
	.constword "DOVOC",DOVOCCON,DOVOC

	.constword "FORTH-KERNEL-START",FORTHKERNELSTARTCON,ENTRY

	; these are needed by the assembler
	.constword "PUSH",PUSHCON,dpush
	.constword "POP",POPCON,dpop
	.constword "RPUSH",RPUSHCON,rpush
	.constword "RPOP",RPOPCON,rpop
	.constword "EXEC",EXECCON,EXEC
	.constword "NEXT",NEXTCON,NEXT
	.constword "PUSHNEXT",PUSHNEXTCON,PUSHNEXT
	.constword "IP",IPCON,IP
	.constword "SP",SPCON,SP
	.constword "RP",RPCON,RP
	.constword "CFA",CFACON,CFA
	.constword "RCFA",RCFACON,RCFA

;*****************************************************************************
; Runtime Primitives
;*****************************************************************************

	.codeword "UNNEST",UNNEST
	jsr rpop
	sta IP
	stx IP+1
	jmp NEXT

	.codeword "(EXIT)",PEXITP
	bra UNNEST+2

	.codeword "BRANCH",BRANCH
	ldy #1
	lda (IP),y
	tax
	lda (IP)
	sta IP
	stx IP+1
	jmp NEXT

	.codeword "?BRANCH",QBRANCH
	jsr dpop
	cpa #0
	bne +
	cpx #0
	bne +
	bra BRANCH+2
+	jsr incIP
	jmp NEXT

	.codeword "(BEGIN)",PBEGINP
	jmp NEXT

	.codeword "(THEN)",PTHENP
	jmp NEXT

	.codeword "(UNTIL)",PUNTILP
	jmp QBRANCH+2

	.codeword "(AGAIN)",PAGAINP
	jmp BRANCH+2

	.codeword "(WHILE)",PWHILEP
	jmp QBRANCH+2

	.codeword "(REPEAT)",PREPEATP
	jmp BRANCH+2

	.codeword "(CASE)",PCASEP
	jmp NEXT

	.codeword "(OF)",POFP
	jsr dpopToGP0
	jsr dpop
	cpa GP0
	bne +
	cpx GP0+1
	bne +
	jsr incIP
	bra ++
+	jsr dpush
	lda (IP)
	sta GP0
	ldy #1
	lda (IP),y
	sta IP+1
	lda GP0
	sta IP
+	jmp NEXT

	.codeword "(ENDOF)",PENDOFP
	lda (IP)
	sta GP0
	ldy #1
	lda (IP),y
	sta IP+1
	lda GP0
	sta IP
	jmp NEXT

	.codeword "(ENDCASE)",PENDCASEP
	jsr incSP
	jmp NEXT

	.codeword "(LOOP)",PLOOPP
	ldy #2
	clc
	lda (RP),y
	adc #1
	sta (RP),y
	iny
	lda (RP),y
	adc #0
	sta (RP),y
	lda (RP),y
	bmi +
	lda (IP)
	sta GP0
	ldy #1
	lda (IP),y
	sta IP+1
	lda GP0
	sta IP
	jmp NEXT
+	jsr incIP
	clc
	lda RP
	adc #6
	sta RP
	bcc +
	inc RP+1
+	jmp NEXT

	.codeword "(+LOOP)",PPLUSLOOPP
	jsr dpopToGP0
	ldy #2
	clc
	lda (RP),y
	adc GP0
	sta (RP),y
	iny
	lda (RP),y
	adc GP0+1
	sta (RP),y
	lda (RP),y
	bmi +
	lda (IP)
	sta GP0
	ldy #1
	lda (IP),y
	sta IP+1
	lda GP0
	sta IP
	bra ++
+	jsr incIP
	clc
	lda RP
	adc #6
	sta RP
	bcc +
	inc RP+1
+	jmp NEXT

	.codeword "(DO)",PDOP
PDOPx
	jsr dpopToGP0
	jsr dpopToGP1
-	ldy #1
	lda (IP),y
	tax
	lda (IP)
	jsr rpush
	jsr incIP
	clc
	lda #$80
	adc GP1+1
	sta GP1+1
	lda GP1
	ldx GP1+1
	jsr rpush
	sec
	lda GP0
	sbc GP1
	tay
	lda GP0+1
	sbc GP1+1
	tax
	tya
	jsr rpush
	jmp NEXT

	.codeword "(?DO)",PQDOP
	jsr dpopToGP0
	jsr dpopToGP1
	lda GP0
	cpa GP1
	bne +
	lda GP0+1
	cpa GP1+1
	beq ++
+	jmp -
+	lda (IP)
	sta GP0
	ldy #1
	lda (IP),y
	sta IP+1
	lda GP0
	sta IP
	jmp NEXT

	.codeword "((LOCALALLOC))",PPLOCALALLOCPP
	jsr dpop
	sta GP0
	stx GP0+1
	lda LP+2
	ldx LP+3
	jsr rpush
	lda RP
	sta LP+2
	lda RP+1
	sta LP+3
	sec
	lda RP
	sbc GP0
	sta RP
	lda RP+1
	sbc GP0+1
	sta RP+1
	lda RP
	ldx RP+1
	jsr dpush
	jsr decRP
	jmp NEXT

	.deferword "(LOCALALLOC)",PLOCALALLOCP,PPLOCALALLOCPP

	.codeword "((LOCALFREE))",PPLOCALFREEPP
	lda LP+2
	sta RP
	lda LP+3
	sta RP+1
	jsr rpop
	sta LP+2
	stx LP+3
	jmp NEXT

	.deferword "(LOCALFREE)",PLOCALFREEP,PPLOCALFREEPP

	.codeword "LIT",LIT
	ldy #1
	lda (IP),y
	tax
	lda (IP)
	jsr dpush
	jsr incIP
	jmp NEXT

	.codeword "PERFORM",PERFORM
	jsr dpopToGP0
	lda (GP0)
	sta CFA
	ldy #1
	lda (GP0),y
	sta CFA+1
	jmp EXEC

	.codeword "EXECUTE",EXECUTE
	jsr dpopToCFA
	jmp EXEC

	.codeword "LEAVE",LEAVE
	clc
	lda RP
	adc #6
	sta RP
	bcc +
	inc RP+1
+	lda (RP)
	sta IP
	ldy #1
	lda (RP),y
	sta IP+1
	jmp NEXT

	.codeword "?LEAVE",QLEAVE
	jsr dpop
	cpa #0
	beq +
	cpx #0
	beq +
	jmp LEAVE+2
+	jmp NEXT

	.codeword "I",I
	ldy #2
	lda (RP),y
	sta GP0
	iny
	lda (RP),y
	sta GP0+1
	iny
	lda (RP),y
	sta GP1
	iny
	lda (RP),y
	sta GP1+1
	clc
	lda GP0
	adc GP1
	sta GP0
	lda GP0+1
	adc GP1+1
	sta GP0+1
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.codeword "J",J
	ldy #8
	lda (RP),y
	sta GP0
	iny
	lda (RP),y
	sta GP0+1
	iny
	lda (RP),y
	sta GP1
	iny
	lda (RP),y
	sta GP1+1
	clc
	lda GP0
	adc GP1
	sta GP0
	lda GP0+1
	adc GP1+1
	sta GP0+1
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.codeword "UNLOOP",UNLOOP
	clc
	lda RP
	adc #6
	sta RP
	bcc +
	lda RP+1
	adc #0
	sta RP+1
+	jmp NEXT

	.codeword "NOOP",NOOP
	jmp NEXT

	.codeword "BYE",BYE
	brk #0
	jmp NEXT

	.codeword "HALT",HALT
	stp

	.codeword '(("))',PPQUOTEPP
	jsr rpop
	sta GP0
	stx GP0+1
	jsr dpush
	lda (GP0)			; get string length
	clc
	adc GP0				; add it to return address
	sta GP0
	bcc +
	inc GP0+1
+	inc GP0				; add 1 for count
	bne +
	inc GP0+1
+	lda GP0
	ldx GP0+1
	jsr rpush
	jmp NEXT

	.colonword '(.")',PDOTQUOTEP
	.word PPQUOTEPP,COUNT,DOTYPE
	.word UNNEST

	.colonword '(S")',PSQUOTEP
	.word PPQUOTEPP,COUNT
	.word UNNEST

	.colonword "_(",UNDERPAREN
	.word LIT,')',PARSE,TWODROP
	.word UNNEST

	.immedcolonword ".(",DOTPAREN
	.word LIT,')',PARSE,DOTYPE
	.word UNNEST

	.varword "HLD",HLD
	.fill 82,0

	.valueword "PAD",PAD,HLD+80+2

	.codeword "B>C",BTOC
	ldy #2
	lda (SP),y
	cpa #10
	blt +
	clc
	adc #7
+	clc
	adc #'0'
	sta (SP),y
	lda #0
	iny
	sta (SP),y
	jmp NEXT

	.colonword "HOLD",HOLD
	.word HLD,FETCH,ONEMINUS,DUP,HLD,STORE,CSTORE
	.word UNNEST

	.colonword "SIGN",DOSIGN
	.word ZLESS,QBRANCH,+
		.word LIT,'-',HOLD
	.word PTHENP
+	.word UNNEST

	.colonword "<#",BEGNUMBER
	.word PAD,HLD,STORE
	.word UNNEST

	.colonword "#",NUMBERSIGN
	.word BASE,FETCH,ZERO,UDSLASHMOD,TWOSWAP,DROP,BTOC,HOLD
	.word UNNEST
					
	.colonword "#S",NUMBERSIGNS
NUMBERSIGNSx
	.word PBEGINP
_L1:	.word NUMBERSIGN,TWODUP,DOOR,ZEQUAL
	.word QBRANCH,_L1
	.word UNNEST

	.colonword "#>",NUMBEREND
	.word TWODROP,HLD,FETCH,PAD,OVER,MINUS
	.word UNNEST

	.colonword "(D.)",PDDOTP
	.word TUCK,DODABS,BEGNUMBER,NUMBERSIGNS,ROT,DOSIGN,NUMBEREND
	.word UNNEST

	.colonword ".",DOT
	.word STOD,DDOT
	.word UNNEST

	.colonword "D.",DDOT
	.word PDDOTP,DOTYPE,SPACE
	.word UNNEST

	.colonword "U.",UDOT
	.word ZERO,DDOT
	.word UNNEST

;*****************************************************************************
; Stack Manipulation Words
;*****************************************************************************

	.varword "SP0",SP0
	.word 0

	.varword "RP0",RP0
	.word 0

	.codeword "SP@",SPFETCH
	lda SP
	ldx SP+1
	jmp PUSHNEXT

	.codeword "SP!",SPSTORE
	jsr dpop
	sta SP
	stx SP+1
	jmp NEXT

	.codeword "RP@",RPFETCH
	lda RP
	ldx RP+1
	jmp PUSHNEXT

	.codeword "RP!",RPSTORE
	jsr dpop
	sta RP
	stx RP+1
	jmp NEXT

	.codeword ">R",TOR
	jsr dpop
	jsr rpush
	jmp NEXT

	.codeword "DUP>R",DUPTOR
	ldy #3
	lda (SP),y
	tax
	dey
	lda (SP),y
	jsr rpush
	jmp NEXT

	.codeword "R@",RFETCH
	ldy #3
	lda (RP),y
	tax
	dey
	lda (RP),y
	jmp PUSHNEXT

	.codeword "R>",RFROM
	jsr rpop
	jmp PUSHNEXT

	.codeword "R>DROP",RFROMDROP
	jsr incRP
	jmp NEXT

	.codeword "2>R",TWOTOR
	jsr dpopToGP0
	jsr dpop
	jsr rpush
	lda GP0
	ldx GP0+1
	jsr rpush
	jmp NEXT

	.codeword "2R>",TWORFROM
	jsr rpop
	sta GP0
	stx GP0+1
	jsr rpop
	jsr dpush
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.codeword "2R@",TWORFETCH
	ldy #5
	lda (RP),y
	tax
	dey
	lda (RP),y
	jsr dpush
	ldy #3
	lda (RP),y
	tax
	dey
	lda (RP),y
	jmp PUSHNEXT

doDup .proc
	ldy #2
	lda (SP),y
	sta (SP)
	iny
	lda (SP),y
	ldy #1
	sta (SP),y
	jsr decSP
	rts
	.endproc

	.codeword "DUP",DUP
	jsr doDup
	jmp NEXT

	.codeword "?DUP",QDUP
	ldy #2
	lda (SP),y
	iny
	ora (SP),y
	beq +
	jsr doDup
+	jmp NEXT

	.codeword "DROP",DROP
	jsr incSP
	jmp NEXT

	.codeword "SWAP",SWAP
	ldy #2
	lda (SP),y
	sta GP0
	iny
	lda (SP),y
	sta GP0+1
	iny
	lda (SP),y
	sta GP1
	iny
	lda (SP),y
	sta GP1+1
	ldy #2
	lda GP1
	sta (SP),y
	iny
	lda GP1+1
	sta (SP),y
	iny
	lda GP0
	sta (SP),y
	iny
	lda GP0+1
	sta (SP),y
	jmp NEXT

	.codeword "OVER",OVER
	ldy #5
	lda (SP),y
	tax
	dey
	lda (SP),y
	jmp PUSHNEXT

	.codeword "ROT",ROT
	ldy #2
	lda (SP),y
	sta GP3
	iny
	lda (SP),y
	sta GP3+1
	iny
	lda (SP),y
	sta GP2
	iny
	lda (SP),y
	sta GP2+1
	iny
	lda (SP),y
	sta GP1
	iny
	lda (SP),y
	sta GP1+1
	ldy #2
	lda GP1
	sta (SP),y
	iny
	lda GP1+1
	sta (SP),y
	iny
	lda GP3
	sta (SP),y
	iny
	lda GP3+1
	sta (SP),y
	iny
	lda GP2
	sta (SP),y
	iny
	lda GP2+1
	sta (SP),y
	jmp NEXT

	.codeword "-ROT",NROT
	ldy #2
	lda (SP),y
	sta GP3
	iny
	lda (SP),y
	sta GP3+1
	iny
	lda (SP),y
	sta GP2
	iny
	lda (SP),y
	sta GP2+1
	iny
	lda (SP),y
	sta GP1
	iny
	lda (SP),y
	sta GP1+1
	ldy #2
	lda GP2
	sta (SP),y
	iny
	lda GP2+1
	sta (SP),y
	iny
	lda GP1
	sta (SP),y
	iny
	lda GP1+1
	sta (SP),y
	iny
	lda GP3
	sta (SP),y
	iny
	lda GP3+1
	sta (SP),y
	jmp NEXT

	.codeword "NIP",NIP
	ldy #2
	lda (SP),y
	pha
	iny
	lda (SP),y
	tax
	iny
	pla
	sta (SP),y
	iny
	txa
	sta (SP),y
	jsr incSP
	jmp NEXT

	; ( n1 n2 -- n2 n1 n2 )
	.colonword "TUCK",TUCK
	.word SWAP,OVER
	.word UNNEST

	.codeword "PICK",PICK
	jsr dpopToGP0
	jsr incGP0
	asl GP0
	rol GP0+1
	clc
	lda SP
	adc GP0
	sta GP0
	lda SP+1
	adc GP0+1
	sta GP0+1
	ldy #1
	lda (GP0),y
	tax
	lda (GP0)
	jmp PUSHNEXT

	.codeword "2DROP",TWODROP
	clc
	lda SP
	adc #4
	sta SP
	bcs +
	jmp NEXT
+	inc SP+1
	jmp NEXT

	.colonword "ROLL",ROLL
	.word TOR,RFETCH,PICK,SPFETCH,CELL,PLUS,DUP,CELL
	.word PLUS,RFROM,CELL,STAR,MOVE,DROP,UNNEST

	.codeword "2DUP",TWODUP
	ldy #5
	lda (SP),y
	sta GP1+1
	dey
	lda (SP),y
	sta GP1
	dey
	lda (SP),y
	sta GP2+1
	dey
	lda (SP),y
	sta GP2
	sec
	lda SP
	sbc #4
	sta SP
	bcs +
	dec SP+1
+	ldy #5
	lda GP1+1
	sta (SP),y
	dey
	lda GP1
	sta (SP),y
	dey
	lda GP2+1
	sta (SP),y
	dey
	lda GP2
	sta (SP),y
	jmp NEXT

	.colonword "2OVER",TWOOVER
	.word LIT,3,PICK,LIT,3,PICK,UNNEST

	.codeword "2SWAP",TWOSWAP
	ldy #2
	lda (SP),y
	sta GP4
	iny
	lda (SP),y
	sta GP4+1
	iny
	lda (SP),y
	sta GP3
	iny
	lda (SP),y
	sta GP3+1
	iny
	lda (SP),y
	sta GP2
	iny
	lda (SP),y
	sta GP2+1
	iny
	lda (SP),y
	sta GP1
	iny
	lda (SP),y
	sta GP1+1
	ldy #2
	lda GP2
	sta (SP),y
	iny
	lda GP2+1
	sta (SP),y
	iny
	lda GP1
	sta (SP),y
	iny
	lda GP1+1
	sta (SP),y
	iny
	lda GP4
	sta (SP),y
	iny
	lda GP4+1
	sta (SP),y
	iny
	lda GP3
	sta (SP),y
	iny
	lda GP3+1
	sta (SP),y
	iny
	jmp NEXT

	.colonword "DEPTH",DEPTH
	.word SPFETCH,SP0,FETCH,SWAP,MINUS,TWO,SLASH
	.word UNNEST

;*****************************************************************************
; Forth Memory Manipulation
;*****************************************************************************

;-----------------------------------------------------------------------------
; Memory Move Routine
; On entry:
;		GP0 = source
;		GP1 = dest
;		GP2 = len
;-----------------------------------------------------------------------------
memMove .proc
	lda GP1+1
	cpa GP0+1
	beq +
	bge moveUp			; dst is higher than src
	bra moveDown		; dst is lower than src
+	lda GP1				; high bytes are equal, check low bytes
	cpa GP0
	beq +
	bge moveUp			; dst is higher than src
	bra moveDown		; dst is lower than src
+	rts
	.endproc

;-----------------------------------------------------------------------------
; Move memory from higher address to lower address
; On entry:
;	GP0 - source
;	GP1 - dest
;	GP2 - len
;-----------------------------------------------------------------------------
moveDown .proc
	ldy #0
	ldx GP2+1			; high byte of length
	beq +
-	lda (GP0),y
	sta (GP1),y
	iny
	bne -
	inc GP0+1
	inc GP1+1
	dex
	bne -
+	ldx GP2
	beq +
-	lda (GP0),y
	sta (GP1),y
	iny
	dex
	bne -
+	rts
	.endproc

;-----------------------------------------------------------------------------
; Move memory from lower address to higher address
; On entry:
;	GP0 - source
;	GP1 - dest
;	GP2 - len
;-----------------------------------------------------------------------------
moveUp .proc
	ldx GP2+1
	clc
	txa
	adc GP0+1
	sta GP0+1
	clc
	txa
	adc GP1+1
	sta GP1+1
	inx
	ldy GP2
	beq ++
	dey
	beq +
-	lda (GP0),y
	sta (GP1),y
	dey
	bne -
+	lda (GP0),y
	sta (GP1),y
+	dey
	dec GP0+1
	dec GP1+1
	dex
	bne -
	rts
	.endproc

	.codeword "!",STORE
	jsr dpopToGP1
	jsr incSP
	lda (SP)
	sta (GP1)
	ldy #1
	lda (SP),y
	sta (GP1),y
	jmp NEXT

	.codeword "+!",PLUSSTORE
	jsr dpopToGP0
	jsr dpop
	clc
	adc (GP0)
	sta (GP0)
	ldy #1
	txa
	adc (GP0),y
	sta (GP0),y
	jmp NEXT

	.codeword "C!",CSTORE
	jsr dpopToGP1
	jsr incSP
	lda (SP)
	sta (GP1)
	jmp NEXT

	.codeword "C+!",CPLUSSTORE
	jsr dpopToGP0
	jsr dpop
	clc
	adc (GP0)
	sta (GP0)
	jmp NEXT

	.codeword "@",FETCH
	ldy #2
	lda (SP),y
	sta GP0
	iny
	lda (SP),y
	sta GP0+1
	lda (GP0)
	ldy #2
	sta (SP),y
	dey
	lda (GP0),y
	ldy #3
	sta (SP),y
	jmp NEXT

	.codeword "C@",CFETCH
	jsr dpopToGP1
	lda (GP1)
	ldx #0
	jmp PUSHNEXT

	.codeword "2!",TWOSTORE
	jsr dpopToGP0
	jsr incSP
	lda (SP)
	sta (GP0)
	ldy #1
	lda (SP),y
	sta (GP0),y
	iny
	lda (SP),y
	sta (GP0),y
	iny
	lda (SP),y
	sta (GP0),y
	jsr incSP
	jmp NEXT

	.codeword "2@",TWOFETCH
	jsr dpopToGP0
	ldy #3
	lda (GP0),y
	tax
	dey
	lda (GP0),y
	jsr dpush
	ldy #1
	lda (GP0),y
	tax
	lda (GP0)
	jmp PUSHNEXT

	.codeword "ON",ON
	jsr dpopToGP1
	lda #$FF
	sta (GP1)
	ldy #1
	sta (GP1),y
	jmp NEXT

	.codeword "OFF",OFF
	jsr dpopToGP1
	lda #0
	sta (GP1)
	ldy #1
	sta (GP1),y
	jmp NEXT

	.colonword "TOGGLE",TOGGLE
	.word DUP,CFETCH,ROT,DOXOR,SWAP,CSTORE
	.word UNNEST

	.codeword "MOVE",MOVE
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP0
	jsr memMove
	jmp NEXT

	.codeword "CMOVE",CMOVE
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP0
	jsr memMove
	jmp NEXT

	.codeword "CMOVE>",CMOVER
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP0
	jsr memMove
	jmp NEXT

	.colonword "INCR",INCR
	.word DUP,FETCH,ONEPLUS,SWAP,STORE
	.word UNNEST

	.colonword "DECR",DECR
	.word DUP,FETCH,ONEMINUS,SWAP,STORE
	.word UNNEST

	.colonword "CINCR",CINCR
	.word DUP,CFETCH,ONEPLUS,SWAP,CSTORE
	.word UNNEST

	.colonword "CDECR",CDECR
	.word DUP,CFETCH,ONEMINUS,SWAP,CSTORE
	.word UNNEST

	.codeword "FILL",FILL
	jsr dpopToGP0
	jsr dpopToGP1
	jsr dpopToGP2
	lda GP0
	ldx GP1+1
	beq +
-	ldy #0
-	sta (GP2),y
	dey
	bne -
	inc GP2+1
	dex
	bne --
+	ldy GP1
-	beq +
	dey
	sta (GP2),y
	bra -
+	jmp NEXT

;*****************************************************************************
; Forth String Words
;*****************************************************************************

	.codeword "CLIP$",CLIPSTRING
	ldy #3
	lda (SP),y
	bpl +
	lda #0			; length negative, so make zero
	sta (SP),y
	dey
	sta (SP),y
	bra ++
+	beq +
	lda #0			; length > 255, so clamp to 255
	sta (SP),y
	dey
	dec a
	sta (SP),y
+	jmp NEXT

	.colonword "PLACE",PLACE
	.word SWAP,CLIPSTRING,SWAP,TWODUP,TWOTOR,CHARPLUS,SWAP
	.word MOVE,TWORFROM,CSTORE
	.word UNNEST

	.colonword "+PLACE",PLUSPLACE
	.word TOR,CLIPSTRING,MAXCOUNTED,RFETCH,CFETCH,MINUS,MIN
	.word RFROM,TWODUP,TWOTOR,COUNT,CHARS,PLUS,SWAP,MOVE
	.word TWORFROM,CPLUSSTORE
	.word UNNEST

	.codeword "COUNT",COUNT
	ldy #2
	lda (SP),y
	sta GP0
	iny
	lda (SP),y
	sta GP0+1
	lda (GP0)
	tax
	inc GP0
	bne +
	inc GP0+1
+	lda GP0+1
	sta (SP),y
	dey
	lda GP0
	sta (SP),y
	txa
	ldx #0
	jmp PUSHNEXT

	.codeword "SKIP",SKIP
	jsr dpopToGP0
	jsr dpopToGP1
	jsr dpopToGP2
-	lda GP1
	ora GP1+1
	beq +
	lda (GP2)
	cpa GP0
	bne +
	jsr incGP2
	jsr decGP1
	bra -
+	lda GP2
	ldx GP2+1
	jsr dpush
	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

	; ( addr len char -- addr' len' )
	; scan string at addr/len for first occurance of char
	.codeword "SCAN",SCAN
	jsr dpopToGP0
	jsr dpopToGP1
	jsr dpopToGP2
-	lda GP1
	ora GP1+1
	beq +
	lda (GP2)
	cpa GP0
	beq +
	jsr incGP2
	jsr decGP1
	bra -
+	lda GP2
	ldx GP2+1
	jsr dpush
	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

;-----------------------------------------------------------------------------
; Convert string to uppercase
;
; On Input:
;	GP1 - straddr
;	GP2 - length
;-----------------------------------------------------------------------------
doUpperCase .proc
-	lda GP2
	ora GP2+1
	beq ++
	lda (GP1)
	cmp #'a'-1
	blt +
	cmp #'z'+1
	bge +
	and #$df
	sta (GP1)
+	jsr incGP1
	jsr decGP2
	bra -
+	rts
	.endproc

	; ( addr u -- )
	.codeword "UPPER",UPPER
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doUpperCase
	jmp NEXT

	; ( caddr -- caddr )
	.codeword "UPPERCASE",UPPERCASE
	ldy #2
	lda (SP),y
	sta GP1
	iny
	lda (SP),y
	sta GP1+1
	lda (GP1)
	sta GP2
	stz GP2+1
	jsr incGP1
	jsr doUpperCase
	jmp NEXT

	; ( addr1 u1 n -- addr2 u2 )
	.colonword "/STRING",SLASHSTRING
	.word OVER,MIN,DUPTOR,MINUS,SWAP,RFROM,CHARS,PLUS,SWAP
	.word UNNEST

;-----------------------------------------------------------------------------
; Compare two strings
; Input:
;	GP1	- addr1
;	GP2 - len1
;	GP3 - addr2
;	GP4 - len2
; Output:
;	Equal - a,x == 0,0
;	Str1 Short - a,x = -1,-1
;	Str2 Short - a,x = 1,0
; Uses:
;	GP0
;-----------------------------------------------------------------------------
doCompare .proc
	lda GP2
	eor GP4
	bne _notEq
	lda GP2+1
	eor GP4+1
	bne _notEq
	bra _L2
_loop:
	lda (GP1)
	cpa (GP3)
	beq _continue
	bra _notEq
_continue:
	jsr decGP2
	jsr decGP4
	jsr incGP1
	jsr incGP3
_L2:
	lda GP2
	ora GP2+1
	sta GP0				; u1 status
	lda GP4
	ora GP4+1
	sta GP0+1			; u2 status
	ldx #0
	lda GP0
	ora GP0+1
	beq _done			; both are zero
	lda GP0
	beq _short1			; 1 is shorter
	lda GP0+1
	beq _short2			; 2 is shorter
	bra _loop			; more characters
_notEq:
	bcs _short2
_short1:
	lda #-1
	tax
	bra _done
_short2:
	lda #1
	ldx #0
_done:
	rts
	.endproc

	.codeword "COMPARE",COMPARE
	jsr dpopToGP4
	jsr dpopToGP3
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doCompare
	jmp PUSHNEXT

;-----------------------------------------------------------------------------
; Unsigned Multiply and Divide
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Multiply 32-bit unsigned number by a 32-bit unsigned number
; On Entry:
;	ud1 - GP2 (lo), GP3 (hi)
;	ud2 - GP0 (lo), GP1 (hi)
; On Exit:
;	uq3 - GP4 (lo), GP5 (hi)
;   uq3 - GP6 (lo2),GP7 (hi2)
;-----------------------------------------------------------------------------
mul32x32 .proc
	mul32x32mulnd = GP2
	mul32x32mulr  = GP0
	mul32x32prod  = GP4

	stz mul32x32prod+0
	stz mul32x32prod+1
	stz mul32x32prod+2
	stz mul32x32prod+3
	stz mul32x32prod+4
	stz mul32x32prod+5
	stz mul32x32prod+6
	stz mul32x32prod+7
	clc
	lda #0
	ldx #32
shift:
	lsr mul32x32mulr+3
	ror mul32x32mulr+2
	ror mul32x32mulr+1
	ror mul32x32mulr+0
	bcc rotate
	clc
	lda mul32x32prod+4
	adc mul32x32mulnd
	sta mul32x32prod+4
	lda mul32x32prod+5
	adc mul32x32mulnd+1
	sta mul32x32prod+5
	lda mul32x32prod+6
	adc mul32x32mulnd+2
	sta mul32x32prod+6
	lda mul32x32prod+7
	adc mul32x32mulnd+3
rotate:
	ror a
	sta mul32x32prod+7
	ror mul32x32prod+6
	ror mul32x32prod+5
	ror mul32x32prod+4
	ror mul32x32prod+3
	ror mul32x32prod+2
	ror mul32x32prod+1
	ror mul32x32prod
	dex
	bne shift
	clc
	rts
	.endproc

;-----------------------------------------------------------------------------
; Multiply 16-bit unsigned number by a 16-bit unsigned number, 32-bit result
; On Entry:
;	u1 - GP2 (lo)
;	u2 - GP0 (lo)
; On Exit:
;	ud - GP4 (lo), GP5 (hi)
;-----------------------------------------------------------------------------
mul16x16 .proc
	mul16x16mulnd = GP2
	mul16x16mulr  = GP0
	mul16x16prod  = GP4

	stz mul16x16prod+0
	stz mul16x16prod+1
	stz mul16x16prod+2
	stz mul16x16prod+3
	clc
	lda #0
	ldx #16
shift:
	ror mul16x16mulr+1
	ror mul16x16mulr+0
	bcc rotate
	clc
	lda mul16x16prod+2
	adc mul16x16mulnd
	sta mul16x16prod+2
	lda mul16x16prod+3
	adc mul16x16mulnd+1
rotate:
	ror a
	sta mul16x16prod+3
	ror mul16x16prod+2
	ror mul16x16prod+1
	ror mul16x16prod
	dex
	bne shift
	clc
	rts
	.endproc

;-----------------------------------------------------------------------------
; Divide 32-bit number by a 32-bit number (unsigned)
; input:
;	GP0-GP1 = d1 (dividend)
;	GP2-GP3 = d2 (divisor)
; output:
;	GP0-GP1 = quot
;   GP4-GP5 = rem
; uses:
;   GP6-GP7 = temp
;-----------------------------------------------------------------------------
div32x32 .proc
	div32x32dividend = GP0
	div32x32divisor  = GP2
	div32x32rem      = GP4
	div32x32temp     = GP6

	stz div32x32rem
	stz div32x32rem+1
	stz div32x32rem+2
	stz div32x32rem+3
	ldx #32
-	asl div32x32dividend
	rol div32x32dividend+1
	rol div32x32dividend+2
	rol div32x32dividend+3
	rol div32x32rem
	rol div32x32rem+1
	rol div32x32rem+2
	rol div32x32rem+3
	lda div32x32rem
	sec
	sbc div32x32divisor
	tay
	lda div32x32rem+1
	sbc div32x32divisor+1
	sta div32x32temp
	lda div32x32rem+2
	sbc div32x32divisor+2
	sta div32x32temp+1
	lda div32x32rem+3
	sbc div32x32divisor+3
	bcc +
	sta div32x32rem+3
	lda div32x32temp+1
	sta div32x32rem+2
	lda div32x32temp
	sta div32x32rem+1
	sty div32x32rem
	inc div32x32dividend
+	dex
	bne -
	rts
	.endproc

;-----------------------------------------------------------------------------
; Divide 16-bit number by a 16-bit number (unsigned)
; input:
;	GP0 = d1 (dividend)
;	GP2 = d2 (divisor)
; output:
;	GP0 = quot
;   GP4 = rem
; uses:
;   GP6 = temp
;-----------------------------------------------------------------------------
div16x16 .proc
	div16x16dividend = GP0
	div16x16divisor  = GP2
	div16x16rem      = GP4
	div16x16temp     = GP6

	stz div16x16rem
	stz div16x16rem+1
	ldx #16
-	asl div16x16dividend
	rol div16x16dividend+1
	rol div16x16rem
	rol div16x16rem+1
	lda div16x16rem
	sec
	sbc div16x16divisor
	tay
	lda div16x16rem+1
	sbc div16x16divisor+1
	bcc +
	sta div16x16rem+1
	sty div16x16rem
	inc div16x16dividend
+	dex
	bne -
	rts
	.endproc

	.codeword "+",PLUS
	clc
	ldy #2
	lda (SP),y
	ldy #4
	adc (SP),y
	sta (SP),y
	ldy #3
	lda (SP),y
	ldy #5
	adc (SP),y
	sta (SP),y
	jsr incSP
	jmp NEXT

	.codeword "D+",DPLUS
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP4
	jsr dpopToGP3
	clc
	lda GP1
	adc GP3
	sta GP5
	lda GP1+1
	adc GP3+1
	sta GP5+1
	lda GP2
	adc GP4
	sta GP6
	lda GP2+1
	adc GP4+1
	sta GP6+1
	lda GP5
	ldx GP5+1
	jsr dpush
	lda GP6
	ldx GP6+1
	jmp PUSHNEXT

	.codeword "-",MINUS
	sec
	ldy #4
	lda (SP),y
	ldy #2
	sbc (SP),y
	ldy #4
	sta (SP),y
	ldy #5
	lda (SP),y
	ldy #3
	sbc (SP),y
	ldy #5
	sta (SP),y
	jsr incSP
	jmp NEXT

	.codeword "D-",DMINUS
	jsr dpopToGP4
	jsr dpopToGP3
	jsr dpopToGP2
	jsr dpopToGP1
	sec
	lda GP1
	sbc GP3
	sta GP5
	lda GP1+1
	sbc GP3+1
	sta GP5+1
	lda GP2
	sbc GP4
	sta GP6
	lda GP2+1
	sbc GP4+1
	sta GP6+1
	lda GP5
	ldx GP5+1
	jsr dpush
	lda GP6
	ldx GP6+1
	jmp PUSHNEXT

	.codeword "1+",ONEPLUS
	ldy #2
	lda (SP),y
	inc a
	sta (SP),y
	bne +
	iny
	lda (SP),y
	inc a
	sta (SP),y
+	jmp NEXT

	.deferword "CHAR+",CHARPLUS,ONEPLUS

	.codeword "CHARS",CHARS
	jmp NEXT

	.codeword "1-",ONEMINUS
	ldy #2
	sec
	lda (SP),y
	sbc #1
	sta (SP),y
	bcs +
	iny
	lda (SP),y
	dec a
	sta (SP),y
+	jmp NEXT

	.deferword "CHAR-",CHARMINUS,ONEMINUS

	.codeword "2+",TWOPLUS
	ldy #2
	clc
	lda (SP),y
	adc #2
	sta (SP),y
	bcc +
	iny
	lda (SP),y
	inc a
	sta (SP),y
+	jmp NEXT

	.codeword "2-",TWOMINUS
	ldy #2
	sec
	lda (SP),y
	sbc #2
	sta (SP),y
	bcs +
	iny
	lda (SP),y
	dec a
	sta (SP),y
+	jmp NEXT

	.codeword "2*",TWOSTAR
	ldy #2
	lda (SP),y
	asl a
	sta (SP),y
	iny
	lda (SP),y
	rol a
	sta (SP),y
	jmp NEXT

	.codeword "2/",TWOSLASH
	ldy #3
	lda (SP),y
	cmp #$80
	ror a
	sta (SP),y
	dey
	lda (SP),y
	ror a
	sta (SP),y
	jmp NEXT

	.codeword "LSHIFT",LSHIFT
	jsr dpopToGP0
	jsr dpopToGP1
	clc
	ldx GP0
-	beq +
	asl GP1
	rol GP1+1
	dex
	bra -
+	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

	.codeword "RSHIFT",RSHIFT
	jsr dpopToGP0
	jsr dpopToGP1
	clc
	ldx GP0
-	beq +
	lsr GP1+1
	ror GP1
	dex
	bra -
+	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

	.codeword "=",EQUAL
	jsr incSP
	ldx #0
	ldy #1
	lda (SP),y
	ldy #3
	eor (SP),y
	bne +
	lda (SP)
	ldy #2
	eor (SP),y
	bne +
	dex
+	txa
	ldy #2
	sta (SP),y
	iny
	sta (SP),y
	jmp NEXT

	.colonword "<>",NOTEQUAL
	.word EQUAL,ZEQUAL
	.word UNNEST

	.codeword "0<",ZLESS
	ldx #0
	ldy #3
	lda (SP),y
	and #$80
	beq +
	dex
+	txa
	sta (SP),y
	dey
	sta (SP),y
	jmp NEXT

	.codeword "0=",ZEQUAL
	ldx #0
	ldy #2
	lda (SP),y
	iny
	ora (SP),y
	bne +
	dex
+	txa
	sta (SP),y
	dey
	sta (SP),y
	jmp NEXT

	.codeword "0>",ZGREATER
	ldx #0
	ldy #3
	lda (SP),y
	bmi ++		; hi is negative, so not 0>
	bne +		; hi is positive, so is 0>
	dey
	lda (SP),y
	beq ++		; lo is zero, so not 0>
+	dex
+	txa
	ldy #3
	sta (SP),y
	dey
	sta (SP),y
	jmp NEXT

	.colonword "<",LESS
	.word MINUS,ZLESS,UNNEST

	.colonword ">",GREATER
	.word SWAP,MINUS,ZLESS,UNNEST

	.codeword "U<",ULESS
	jsr dpopToGP2
	jsr dpopToGP1
	ldx #$ff
	lda GP1+1
	cpa GP2+1
	bcc ++
	bne +
	lda GP1
	cpa GP2
	bcc ++
+	inx
+	txa
	jmp PUSHNEXT

	.colonword "U>",UGREATER
	.word SWAP,ULESS
	.word UNNEST

	.codeword "NEGATE",NEGATE
	ldy #2
	sec
	lda #0
	sbc (SP),y
	sta (SP),y
	iny
	lda #0
	sbc (SP),y
	sta (SP),y
	jmp NEXT

	.codeword "DNEGATE",DNEGATE
	jsr dpopToGP1
	jsr dpopToGP0
	sec
	lda #0
	sbc GP0
	sta GP0
	lda #0
	sbc GP0+1
	sta GP0+1
	lda #0
	sbc GP1
	sta GP1
	lda #0
	sbc GP1+1
	sta GP1+1
	lda GP0
	ldx GP0+1
	jsr dpush
	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

	.codeword "AND",DOAND
	jsr dpopToGP0
	jsr dpopToGP1
	lda GP0+1
	and GP1+1
	tax
	lda GP0
	and GP1
	jmp PUSHNEXT

	.codeword "OR",DOOR
	jsr dpopToGP0
	jsr dpopToGP1
	lda GP0+1
	ora GP1+1
	tax
	lda GP0
	ora GP1
	jmp PUSHNEXT

	.codeword "XOR",DOXOR
	jsr dpopToGP0
	jsr dpopToGP1
	lda GP0+1
	eor GP1+1
	tax
	lda GP0
	eor GP1
	jmp PUSHNEXT

	.codeword "INVERT",INVERT
	lda #-1
	tax
	jsr dpush
	jmp DOXOR+2

	.colonword "+-",PLUSMINUS
	.word ZLESS,QBRANCH,+,NEGATE
+	.word UNNEST

	.colonword "D+-",DPLUSMINUS
	.word ZLESS,QBRANCH,+,DNEGATE
+	.word UNNEST

	.colonword "ABS",DOABS
	.word DUP,PLUSMINUS
	.word UNNEST

	.colonword "DABS",DODABS
	.word DUP,DPLUSMINUS
	.word UNNEST

	.codeword "U*",USTAR
	jsr dpopToGP0
	jsr dpopToGP2
	jsr mul16x16
	lda GP4
	ldx GP4+1
	jmp PUSHNEXT

	.codeword "UM*",UMSTAR
	jsr dpopToGP0
	jsr dpopToGP2
	jsr mul16x16
	lda GP4
	ldx GP4+1
	jsr dpush
	lda GP5
	ldx GP5+1
	jmp PUSHNEXT

	.codeword "UD*",UDSTAR
	jsr dpopToGP1
	jsr dpopToGP0
	jsr dpopToGP3
	jsr dpopToGP2
	jsr mul32x32
	lda GP4
	ldx GP4+1
	jsr dpush
	lda GP5
	ldx GP5+1
	jmp PUSHNEXT

	.codeword "UD/MOD",UDSLASHMOD
	jsr dpopToGP3
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP0
	jsr div32x32
	lda GP4				; drem
	ldx GP4+1
	jsr dpush
	lda GP5
	ldx GP5+1
	jsr dpush
	lda GP0				; d
	ldx GP0+1
	jsr dpush
	lda GP1
	ldx GP1+1
	jmp PUSHNEXT

	.colonword "*",STAR
	.word TWODUP,DOXOR,TOR,DOABS,SWAP,DOABS,USTAR,RFROM,PLUSMINUS
	.word UNNEST

	.colonword "M*",MSTAR
	.word TWODUP,DOXOR,TOR,DOABS,SWAP,DOABS,UMSTAR,RFROM,DPLUSMINUS
	.word UNNEST

	.codeword "UM/MOD",UMSLASHMOD
	stz GP3
	stz GP3+1
	jsr dpopToGP2
	jsr dpopToGP1
	jsr dpopToGP0
	jsr div32x32
	lda GP4
	ldx GP4+1
	jsr dpush
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.colonword "UM/",UMSLASH
	.word UMSLASHMOD,SWAP,DROP,UNNEST

	.colonword "UMMOD",UMMOD
	.word UMSLASHMOD,DROP,UNNEST

	.codeword "U/",USLASH
	jsr dpopToGP2
	jsr dpopToGP0
	jsr div16x16
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.codeword "U/MOD",USLASHMOD
	jsr dpopToGP2
	jsr dpopToGP0
	jsr div16x16
	lda GP4
	ldx GP4+1
	jsr dpush
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.codeword "UMOD",UMOD
	jsr dpopToGP2
	jsr dpopToGP0
	jsr div16x16
	lda GP4
	ldx GP4+1
	jmp PUSHNEXT

	.colonword "/",SLASH
	.word TWODUP,DOXOR,TOR,DOABS,USLASH,RFROM,PLUSMINUS
	.word UNNEST

	.colonword "MOD",MOD
	.word TWODUP,DOXOR,TOR,DOABS,UMOD,RFROM,PLUSMINUS
	.word UNNEST

	.colonword "/MOD",SLASHMOD
	.word TWODUP,DOXOR,TOR,DOABS,USLASHMOD,RFROM,PLUSMINUS
	.word UNNEST

	.colonword "S>D",STOD
	.word DUP,ZLESS
	.word UNNEST

	.colonword "U>D",UTOD
	.word ZERO
	.word UNNEST

	.colonword "*/MOD",STARSLASHMOD
	.word TOR,MSTAR,DUP,ZLESS,TOR
	.word DODABS,RFROM,RFROM,SWAP,TOR,DUP,ZLESS,TOR,DOABS
	.word UMSLASHMOD,RFROM,RFROM,DOXOR,PLUSMINUS
	.word UNNEST

	.colonword "*/",STARSLASH
	.word STARSLASHMOD,SWAP,DROP
	.word UNNEST

	.colonword "SM/REM",SMSSLASHREM
	.word DUP,ZLESS,TOR,TOR,DUP,ZLESS,RFROM,SWAP,TOR,TOR
	.word DODABS,RFROM,DOABS,UMSLASHMOD
	.word RFROM,RFROM,DOXOR,PLUSMINUS
	.word UNNEST

	.colonword "MIN",MIN
	.word TWODUP,GREATER
	.word QBRANCH,+,SWAP,PTHENP
+	.word DROP
	.word UNNEST

	.colonword "MAX",MAX
	.word TWODUP,LESS
	.word QBRANCH,+,SWAP,PTHENP
+	.word DROP
	.word UNNEST

	.colonword "0MAX",ZMAX
	.word ZERO,MAX
	.word UNNEST

	.deferword "CELL+",CELLPLUS,TWOPLUS
	.deferword "CELL-",CELLMINUS,TWOMINUS

	.colonword "CELLS",CELLS
	.word TWOSTAR
	.word UNNEST

	.colonword "CELLS+",CELLSPLUS
	.word CELLS,PLUS
	.word UNNEST

	.colonword "CELLS-",CELLSMINUS
	.word CELLS,MINUS
	.word UNNEST

	.colonword "BOUNDS",BOUNDS
	.word OVER,PLUS,SWAP
	.word UNNEST

	.colonword "BETWEEN",BETWEEN
	.word LIT,2,PICK,MIN,TOR,MAX,RFROM,EQUAL
	.word UNNEST

	.colonword "WITHIN",WITHIN
	.word LIT,2,PICK,SWAP,ULESS,NROT,TWODUP,EQUAL,NROT
	.word GREATER,DOOR,DOAND,UNNEST

;*****************************************************************************
; Forth Terminal Support
;*****************************************************************************

	.codeword "(EMIT)",PEMITP
	jsr dpop
	jsr KWRITETERM
	jmp NEXT

	.deferword "EMIT",EMIT,PEMITP

	.codeword "(TYPE)",PTYPEP
	jsr dpopToGP0
	jsr dpopToGP1
-	lda GP0
	ora GP0+1
	beq +
	lda (GP1)
	jsr KWRITETERM
	jsr incGP1
	jsr decGP0
	bra -
+	jmp NEXT

	.deferword "TYPE",DOTYPE,PTYPEP

	.codeword "(CLS)",PCLSP
	jsr KCLS
	jmp NEXT

	.deferword "CLS",CLS,PCLSP

	.codeword "(GOTOXY)",PGOTOXYP
	jsr dpop
	sta GP0
	jsr dpop
	tax
	ldy GP0
	jsr KSETCURXY
	jmp NEXT

	.deferword "GOTOXY",GOTOXY,PGOTOXYP

	.codeword "(GETXY)",PGETXYP
	jsr KGETCURX
	ldx #0
	jsr dpush
	jsr KGETCURY
	ldx #0
	jmp PUSHNEXT

	.deferword "GETXY",GETXY,PGETXYP

	.codeword "(GETCOLROW)",PGETCOLROWP
	jsr KGETSCRW
	ldx #0
	jsr dpush
	jsr KGETSCRH
	ldx #0
	jmp PUSHNEXT

	.deferword "GETCOLROW",GETCOLROW,PGETCOLROWP

	.colonword "COLS",COLS
	.word GETCOLROW,DROP
	.word UNNEST

	.colonword "ROWS",ROWS
	.word GETCOLROW,SWAP,DROP
	.word UNNEST

;-----------------------------------------------------------------------------
; Read a line from the terminal
; On input:
;	addr	- GP2
;	len		- GP1
; On exit:
;	inlen	- GP0
;-----------------------------------------------------------------------------
doAccept .proc
	stz GP0				; n2
	stz GP0+1
nextch:
	jsr CHRIN
	cmp #$08
	beq bksp
	cmp #$7f
	bne notbksp
bksp:
	lda GP0
	ora GP0+1
	beq nextch
	jsr decGP0
	jsr decGP1
	lda #$08
	jsr CHROUT
	bra nextch
notbksp:
	cmp #$0d
	beq done
	cmp #$0a
	beq done
	tax
	lda GP2
	cpa GP0
	bne storeChar
	lda GP2+1
	cpa GP0+1
	bne storeChar
	lda #$07
	jsr CHROUT
	bra nextch
storeChar:
	txa
	sta (GP1)
	jsr CHROUT
	jsr incGP1
	jsr incGP0
	bra nextch
done:
	rts
	.endproc

	; ( a1 n1 -- n2 )
	.codeword "(ACCEPT)",PACCEPTP
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doAccept
	lda GP0
	ldx GP0+1
	jmp PUSHNEXT

	.deferword "ACCEPT",ACCEPT,PACCEPTP

	.codeword "(KEY?)",PKEYQP
	jsr KCHECKTERM
	tax
	jmp PUSHNEXT

	.deferword "KEY?",KEYQ,PKEYQP

	.codeword "(KEY)",PKEYP
	jsr KGETCHAR
	ldx #0
	jmp PUSHNEXT

	.deferword "KEY",KEY,PKEYP

	.colonword "SPACE",SPACE
	.word BL,EMIT
	.word UNNEST

	.constword "SPCS-MAX",SPCSMAX,128
	.varword "SPCS",SPCS
	.fill 128,32

	.colonword "SPACES",SPACES
	.word PBEGINP
-		.word DUP,ZGREATER
	.word PWHILEP,+
		.word DUP,SPCSMAX,MIN,SPCS,OVER,DOTYPE,MINUS
	.word PREPEATP,-
+	.word DROP,UNNEST

	.colonword "(CR)",PCRP
	.word LIT,10,EMIT
	.word UNNEST

	.deferword "CR",CR,PCRP

	.codeword "(MS)",PMSP
	jsr dpop
	sta GP0
	stx GP0+1
	ora GP0+1
	beq +
-	lda #0
	ldy #150
-	cpy #1
	dey
	sbc #0
	bcs -
	jsr decGP0
	lda GP0
	ora GP0+1
	bne --
+	jmp NEXT

	.deferword "MS",MS,PMSP

;=============================================================================
; Forth Exception Word Set
;=============================================================================

	.varword "LP",LP
	.word 0

	.varword "HANDLER",HANDLER
	.word 0

	; ( xt -- f )
	.colonword "CATCH",CATCH
	.word SPFETCH,TOR,LP,FETCH,TOR,HANDLER,FETCH,TOR,RPFETCH,HANDLER
	.word STORE,EXECUTE
	.word RFROM,HANDLER,STORE,RFROM,DROP,RFROM,DROP,ZERO
	.word UNNEST

	; ( n -- )
	.colonword "THROW",THROW
	.word QDUP
	.word QBRANCH,+
	.word	HANDLER,FETCH,RPSTORE,RFROM,HANDLER,STORE,RFROM,LP,STORE
	.word	RFROM,SWAP,TOR,SPSTORE,DROP,RFROM
	.word PTHENP
+	.word UNNEST

	.colonword "?THROW",QTHROW
	.word SWAP
	.word QBRANCH,+
	.word	THROW
	.word BRANCH,++
+	.word	DROP
	.word PTHENP
+	.word UNNEST

	.constword "THROW_ABORT",THROWABORT,THROW_ABORT
	.constword "THROW_ABORT?",THROWABORTQ,THROW_ABORTQ
	.constword "THROW_COMPONLY",THROWCOMPONLY,THROW_COMPONLY
	.constword "THROW_EXECONLY",THROWEXECONLY,THROW_EXECONLY
	.constword "THROW_FILECLOSEFAIL",THROWFILECLOSEFAIL,THROW_FILECLOSEFAIL
	.constword "THROW_FILECREATEFAIL",THROWFILECREATEFAIL,THROW_FILECREATEFAIL
	.constword "THROW_FILENOTFOUND",THROWFILENOTFOUND,THROW_FILENOTFOUND
	.constword "THROW_FILEREADFAIL",THROWFILEREADFAIL,THROW_FILEREADFAIL
	.constword "THROW_FILEWRITEFAIL",THROWFILEWRITEFAIL,THROW_FILEWRITEFAIL
	.constword "THROW_MISMATCH",THROWMISMATCH,THROW_MISMATCH
	.constword "THROW_NAMEREQD",THROWNAMEREQD,THROW_NAMEREQD
	.constword "THROW_NOTDEFER",THROWNOTDEFER,THROW_NOTDEFER
	.constword "THROW_NOTVALUE",THROWNOTVALUE,THROW_NOTVALUE
	.constword "THROW_OUTOFMEM",THROWOUTOFMEM,THROW_OUTOFMEM
	.constword "THROW_STACKCHG",THROWSTACKCHG,THROW_STACKCHG
	.constword "THROW_STACKUNDER",THROWSTACKUNDER,THROW_STACKUNDER
	.constword "THROW_UNDEFINED",THROWUNDEFINED,THROW_UNDEFINED

TMSG01:
	.word 0
	.sint THROW_STACKUNDER
	.ptext "stack underflow"
TMSG02:
	.word TMSG01
	.sint THROW_UNDEFINED
	.ptext "is undefined"
TMSG03:
	.word TMSG02
	.sint THROW_COMPONLY
	.ptext "is compilation only"
TMSG04:
	.word TMSG03
	.sint THROW_NAMEREQD
	.ptext "requires a name"
TMSG05:
	.word TMSG04
	.sint THROW_MISMATCH
	.ptext "control structure mismatch"
TMSG06:
	.word TMSG05
	.sint THROW_FILENOTFOUND
	.ptext "file not found"
TMSG40:
	.word TMSG06
	.sint THROW_NOTDEFER
	.ptext "not defered"
TMSG41:
	.word TMSG40
	.sint THROW_NOTVALUE
	.ptext "not value"
TMSG42:
	.word TMSG41
	.sint THROW_OUTOFMEM
	.ptext "out of memory"
TMSG43:
	.word TMSG42
	.sint THROW_FILECREATEFAIL
	.ptext "file create failed"
TMSG44:
	.word TMSG43
	.sint THROW_FILEREADFAIL
	.ptext "file read failed"
TMSG45:
	.word TMSG44
	.sint THROW_FILEWRITEFAIL
	.ptext "file write failed"
TMSG46:
	.word TMSG45
	.sint THROW_EXECONLY
	.ptext "is execution only"
TMSG47:
	.word TMSG46
	.sint THROW_LOCALSTWICE
	.ptext "locals defined twice"
TMSG48:
	.word TMSG47
	.sint THROW_TOOMANYLOCALS
	.ptext "too many locals"
TMSG49:
	.word TMSG48
	.sint THROW_LOCALSNOCLOSE
	.ptext "locals missing }"
TMSG50:
	.word TMSG49
	.sint THROW_STACKCHG
	.ptext "stack changed"
TMSG51:
	.word TMSG50
	.sint THROW_ARGSNODASHDASH
	.ptext "locals missing --"
TMSG52:
	.word TMSG51
	.sint THROW_NOTLOCAL
	.ptext "is not a LOCAL"
TMSG53:
	.word TMSG52
	.sint THROW_FILECLOSEFAIL
	.ptext "file close failed"

	.varword "THROW_MESSAGES",THROWMSGS
	.word TMSG53

	.varword "NULLMSG",NULLMSG
	.word 0

	.varword "MSG",MSG
	.word NULLMSG

	.varword "PTRNULL",PTRNULL
	.word 0

	.valueword "LAST-ERROR",LASTERROR,0

;*****************************************************************************
; QUERY portion of QUERY-INTERPRET
;*****************************************************************************

	.varword "TIB",TIB
	.fill 256,0

	.varword "(SOURCE)",PSOURCEP
	.word 0
	.word TIB+2

	.colonword "SOURCE",SOURCE
	.word PSOURCEP,TWOFETCH
	.word UNNEST

	.constword "#TIB",HASHTIB,PSOURCEP+2

	.varword ">IN",TOIN
	.word 0

	.valueword "SOURCE-ID",SOURCEID,0
	.valueword "SOURCE-POSITION",SOURCEPOSITION,0

	.codeword "(SAVE-INPUT)",PSAVEINPUTP
	jsr dpop
	jsr rpush
	jsr dpop
	jsr rpush
	jsr dpop
	jsr rpush
	jsr dpop
	jsr rpush
	jsr dpop
	jsr rpush
	jsr dpop
	jsr rpush
	jmp NEXT

	.codeword "(RESTORE-INPUT)",PRESTOREINPUTP
	jsr rpop
	jsr dpush
	jsr rpop
	jsr dpush
	jsr rpop
	jsr dpush
	jsr rpop
	jsr dpush
	jsr rpop
	jsr dpush
	jsr rpop
	jsr dpush
	jmp NEXT

	.colonword "QUERY",QUERY
	.word TIB,DUP,MAXSTRING,ACCEPT,PSOURCEP,TWOSTORE
	.word TOIN,OFF,ZERO,SOURCEID+4,ZERO,SOURCEPOSITION+4
	.word UNNEST

;*****************************************************************************
; Forth Parser
;*****************************************************************************
	.varword "POCKET",POCKET
	.fill 256,0

	.colonword "WORD",DOWORD
	.word TOR,TOIN,FETCH,DUP,PSOURCEP,TWOPLUS,FETCH,PLUS
	.word PSOURCEP,FETCH,ROT,MINUS,DUP,ZGREATER
	.word QBRANCH,+
		.word RFETCH,SKIP,TWODUP,RFROM,SCAN
		.word PSOURCEP,FETCH,OVER,MINUS,ONEPLUS,TOIN,STORE
		.word SWAP,DROP,MINUS
	.word BRANCH,++
+		.word TWODROP,RFROM,DROP,ZERO,ZERO
	.word PTHENP
+	.word POCKET,PLACE,POCKET,UNNEST

	.colonword "PARSE",PARSE
	.word TOR,SOURCE,TOIN,FETCH,SLASHSTRING,TWODUP,RFROM,SCAN
	.word NIP,MINUS,DUP,ONEPLUS,TOIN,PLUSSTORE
	.word UNNEST

;*****************************************************************************
; Number Input
;*****************************************************************************
	.varword "BASE",BASE
	.word 0

	.colonword "DECIMAL",DECIMAL
	.word LIT,10,BASE,STORE
	.word UNNEST

	.colonword "HEX",HEX
	.word LIT,16,BASE,STORE
	.word UNNEST

	.colonword "BINARY",BINARY
	.word LIT,2,BASE,STORE
	.word UNNEST

	.valueword "?DOUBLE",DOUBLEQ,FALSE
	.valueword "DP-LOCATION",DPLOCATION,TRUE

	; ( char base -- n f )
	.codeword "DIGIT",DIGIT
	jsr dpopToGP0
	jsr dpopToGP1
	lda GP1
	sec
	sbc #48
	bcc ++
	cpa #9
	bcc +
	beq +
	;sec		; if !bcc then obviously sec
	sbc #7
	cpa #10
	bcc ++
+	cpa GP0
	bcs +
	ldx #0
	jsr dpush
	lda #-1
	tax
	bra ++
+	lda GP1
	ldx #0
	jsr dpush
	lda #0
	tax
+	jmp PUSHNEXT

	; ( d1 addr len -- d2 addr2 len2 )
	.colonword ">NUMBER",TONUMBER
TONUMBERx ; this is just to get rid of label redefinitions for local labels
	.word TWODUP
	.word BOUNDS
	.word PQDOP,_L1
_L4:	.word OVER,CFETCH,BASE,FETCH,DIGIT
		.word QBRANCH,_L2
			.word NROT,TWOTOR,TOR,BASE,FETCH,STOD,UDSTAR
			.word RFROM,STOD,DPLUS
			.word TWORFROM,SWAP,ONEPLUS,SWAP,ONEMINUS
		.word BRANCH,_L3
_L2:		.word DROP,LEAVE
		.word PTHENP
_L3:.word PLOOPP,_L4
_L1:.word UNNEST

	; ( addr len -- d f )
	.colonword "NUMBER?",NUMBERQ
NUMBERQx ; this is just to get rid of label redefinitions for local labels
	.word FALSECON,DOUBLEQ+4,LIT,$FFFF,DPLOCATION+4
	.word OVER,CFETCH,LIT,'-',EQUAL,OVER,ZGREATER,DOAND,DUPTOR
	.word QBRANCH,_L1
	.word	ONE,SLASHSTRING
	.word PTHENP
_L1:.word DUP,ZEQUAL
	.word QBRANCH,_L2
	.word	RFROMDROP,TWODROP,ZERO,ZERO,FALSECON,PEXITP
	.word PTHENP
_L2:.word ZERO,ZERO,TWOSWAP,TONUMBER,OVER,CFETCH,LIT,'.',EQUAL
	.word OVER,ZGREATER,DOAND
	.word QBRANCH,_L3
	.word	DUP,ONEMINUS,DPLOCATION+4,ONE,SLASHSTRING
	.word	TONUMBER,DUP,ZEQUAL
	.word	QBRANCH,_L4
	.word		TRUECON,DOUBLEQ+4
	.word	PTHENP
_L4:.word PTHENP
_L3:.word NIP,ZEQUAL,RFROM
	.word QBRANCH,_L5
	.word	TOR,DNEGATE,RFROM
	.word PTHENP
_L5:.word UNNEST

	; ( f -- )
	.colonword "?MISSING",QMISSING
	.word THROWUNDEFINED,QTHROW
	.word UNNEST

	; ( str -- d )
	.colonword "(NUMBER)",PNUMBERP
	.word COUNT,FINDBUFFER,PLACE
	.word FINDBUFFER,QUPPERCASE,COUNT,NUMBERQ,ZEQUAL,QMISSING
	.word UNNEST

	.deferword "NUMBER",NUMBER,PNUMBERP

;*****************************************************************************
; Forth Dictionary Support Words
;*****************************************************************************

	; nfa -> lfa
	;
	; input:
	;	a - nfa lo
	;	x - nfa hi
	;
	; output:
	;	a - lfa lo
	;	x - lfa hi
	;
	; uses:
	;	GP0
doNtoLink .proc
	sta GP0
	stx GP0+1
	lda (GP0)
	and #63
	clc
	adc GP0
	sta GP0
	bcc +
	;inc GP0+1
    inx
    clc
+	lda #3
	adc GP0
	sta GP0
	bcc +
	;inc GP0+1
    inx
+	lda GP0
	;ldx GP0+1
	rts
	.endproc

	.codeword "N>LINK",NTOLINK
	jsr dpop
	jsr doNtoLink
	jmp PUSHNEXT

; : NFA-COUNT ( nfa -- addr len ) DUP 1+ SWAP C@ MAX-NAME-CHARS AND ;
	.colonword "NFA-COUNT",NFACOUNT
	.word DUP,ONEPLUS,SWAP,CFETCH,MAXNAMECHARS,DOAND
	.word UNNEST

; : NAME>     ( nfa -- cfa )      NFA-COUNT + 2 CELLS+ ;
	.colonword "NAME>",NAMETO
	.word NFACOUNT,PLUS,TWO,CELLSPLUS
	.word UNNEST

; : >NAME     ( cfa -- nfa )      2 CELLS- @ ;
	.colonword ">NAME",TONAME
	.word TWO,CELLSMINUS,FETCH
	.word UNNEST

; : BODY>     ( pfa -- cfa )      CELL- ;
	.colonword "BODY>",BODYTO
	.word CELLMINUS
	.word UNNEST

; : >BODY     ( cfa -- pfa )      CELL+ ;
	.colonword ">BODY",TOBODY
	.word CELLPLUS
	.word UNNEST

; : VCFA>VOC  ( vcfa -- voc )     2 CELLS+ ;
	.colonword "VCFA>VOC",VCFATOVOC
	.word TWO,CELLSPLUS
	.word UNNEST

; : VOC>VCFA  ( voc -- vcfa )     TWO CELLS- ;
	.colonword "VOC>VCFA",VOCTOVCFA
	.word TWO,CELLSMINUS
	.word UNNEST

; : VLINK>VOC ( vlink -- voc )    CELL+ ;
	.colonword "VLINK>VOC",VLINKTOVOC
	.word CELLPLUS
	.word UNNEST

; : VOC>VLINK ( voc -- vlink )    CELL- ;
	.colonword "VOC>VLINK",VOCTOVLINK
	.word CELLMINUS
	.word UNNEST

;*****************************************************************************
; Forth Dictionary Words
;*****************************************************************************
	.constword "#VOCS",NUMVOCS,NUMVOCSDEF

	.wordheader "FORTH"
FORTH:
	.word DOVOC
	.word 0
	.word COLDNFA
	.word 0

	.varword "CONTEXT",CONTEXT
	.word FORTH+4
	.fill NUMVOCSDEF*2,0

	.varword "CURRENT",CURRENT
	.word FORTH+4

	.varword "FIND-BUFFER",FINDBUFFER
	.fill 256,0

	; input:
	;	GP1 - addr
	;	GP2 - len
	;	GP3 - wlst
	; output:
	;	carry clear -> no match
	;	carry set   -> GP3 - cfa
	;	            -> GP2 - imm
	; uses:
	;	GP4
doSearch1Wordlist .proc
s1wbegin:
	lda GP3						; end of wordlist?
	ora GP3+1
	bne s1w0					; no, continue
	bra s1wno					; yes, no match for you
s1w0:
	lda (GP3)					; len2 = (wlst) & 63
	and #63
	cpa GP2
	bne s1wnext					; not same, so don't match
	clc
	lda GP3
	adc #1
	sta GP5
	lda GP3+1
	adc #0
	sta GP5+1
	ldy GP2
s1w1:
	dey
	bmi s1wmatch
	lda (GP1),y					; (wlst) == (addr2)?
	cpa (GP5),y
	bne s1wnext
	bra s1w1
s1wnext:
	lda GP3						; wlst => lfa
	ldx GP3+1
	jsr doNtoLink
	sta GP3
	stx GP3+1
	ldy #1						; wlst = (lfa)
	lda (GP3),y
	tax
	lda (GP3)
	sta GP3
	stx GP3+1
	bra s1wbegin				; check next word in wordlist
s1wmatch:
	lda (GP3)					; a = (wlst) & 63
	sta GP4						; save for later
	and #63
	clc							; wlst += len
	adc GP3
	sta GP3
	bcc s1wmatcha
	inc GP3+1
    clc
s1wmatcha:
	lda GP3                     ; wlst -> cfa (1 for count, 2 for nla, 2 for lfa
	adc #5
	sta GP3
	bcc s1wmatchb
	inc GP3+1
s1wmatchb:
	sec							; sec -> match
	lda GP4
	and #$80					; immediate?
	bne s1wimmed
	dec a						; regular word
	tax
	bra s1wend
s1wimmed:
	lda #1						; immediate word
	ldx #0
	bra s1wend
s1wno:
	clc							; clc -> no match
s1wend:
	sta GP2
	stx GP2+1
	rts
	.endproc

	; ( addr len wlst -- 0 | cfa 1 | cfa -1 )
	; uses:
	;	GP1 - addr1
	;	GP2 - len1
	;	GP3 - wlst
	.codeword "SEARCH-ONE-WORDLIST",SEARCH1WORDLIST
	jsr dpopToGP3
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doSearch1Wordlist
	bcc +
	lda GP3
	ldx GP3+1
	jsr dpush
	lda GP2
	ldx GP2+1
	bra ++
+	lda #0
	tax
+	jsr dpush
	jmp NEXT

	.colonword "(SEARCH-WORDLIST)",PSEARCHWORDLISTP
	.word FETCH,SEARCH1WORDLIST
	.word UNNEST

	.colonword "(FIND)",PFINDP
	.word DUP,CFETCH,ZEQUAL
	.word QBRANCH,+
	.word	ZERO,PEXITP
	.word PTHENP
+	.word CONTEXT
	.word PBEGINP
-	.word	DUP,FETCH
	.word PWHILEP,+++
	.word	DUP,TWOFETCH,NOTEQUAL
	.word	QBRANCH,++
	.word		OVER,COUNT,MAXNAMECHARS,MIN,LIT,2,PICK,FETCH
	.word		PSEARCHWORDLISTP,QDUP
	.word		QBRANCH,+
	.word			TWOSWAP,TWODROP,PEXITP
	.word		PTHENP
+	.word	PTHENP
+	.word	CELLPLUS
	.word PREPEATP,-
+	.word DROP,FALSECON
	.word UNNEST

	.varword "CAPS",CAPS
	.word $FFFF

	.colonword "?UPPERCASE",QUPPERCASE
	.word CAPS,FETCH
	.word QBRANCH,+
	.word	UPPERCASE
	.word PTHENP
+	.word UNNEST

	; ( cstr -- cstr 0 | cfa 1 | cfa -1 )
	.colonword "CAPS-FIND",CAPSFIND
	.word DUP,COUNT,FINDBUFFER,PLACE,FINDBUFFER,QUPPERCASE,PFINDP
	.word DUPTOR
	.word QBRANCH,+
	.word	NIP
	.word BRANCH,++
+	.word	DROP
	.word PTHENP
+	.word RFROM
	.word UNNEST

	; ( cstr -- cstr 0 | cfa 1 | cfa -1 )
	.deferword "FIND",FIND,CAPSFIND

	.colonword "DEFINED",DEFINED
	.word BL,DOWORD,FIND
	.word UNNEST

	.colonword "'",TICK
	.word DEFINED,ZEQUAL,QMISSING
	.word UNNEST

;*****************************************************************************
; Forth Compile Words
;*****************************************************************************

	.varword "DP",DP
	.word ENDOFKERNEL

	.codeword "APP-HERE",APPHERE
	lda DP+2
	ldx DP+3
	jmp PUSHNEXT

	.deferword "HERE",HERE,APPHERE

	.codeword "APP-COMPILE,",APPCOMPILECOMMA
	lda DP+2
	sta GP0
	lda DP+3
	sta GP0+1
	jsr dpop
	sta (GP0)
	txa
	ldy #1
	sta (GP0),y
	clc
	lda DP+2
	adc #2
	sta DP+2
	bcc +
	inc DP+3
+	jmp NEXT
	
	.codeword "APP-COMPILE",APPCOMPILE
	lda DP+2
	sta GP0
	lda DP+3
	sta GP0+1
	ldy #1
	lda (IP),y
	sta (GP0),y
	lda (IP)
	sta (GP0)
	clc
	lda DP+2
	adc #2
	sta DP+2
	bcc +
	inc DP+3
	clc
+	lda IP
	adc #2
	sta IP
	bcs +
	jmp NEXT
+	inc IP+1
	jmp NEXT

	.deferword "COMPILE",COMPILE,APPCOMPILE

	; ( n -- )
	.colonword "APP-,",APPCOMMA
	.word HERE,STORE,TWO,DP,PLUSSTORE
	.word UNNEST

	; ( n -- )
	.deferword ",",COMMA,APPCOMMA

	; ( c -- )
	.colonword "APP-C,",APPCCOMMA
	.word HERE,CSTORE,ONE,DP,PLUSSTORE
	.word UNNEST

	; ( c -- )
	.deferword "C,",CCOMMA,APPCCOMMA

	.colonword "LINK,",LINKCOMMA
	.word HERE,OVER,FETCH,COMMA,SWAP,STORE
	.word UNNEST

	.colonword ',"',COMMAQUOTE
	.word LIT,'"',PARSE,HERE,TOR,DUP,CCOMMA
	.word DUP,ALLOT,RFROM,ONEPLUS,SWAP,MOVE,ALIGN,UNNEST

	.immedcolonword "LITERAL",LITERAL
	.word COMPILE,LIT,COMMA
	.word UNNEST

	.varword "NEW$",NEWSTRING
	.fill 256,0

	.immedcolonword 'S"',SQUOTE
	.word STATE,FETCH
	.word QBRANCH,L1
		.word COMPILE,PSQUOTEP,COMMAQUOTE
	.word BRANCH,L2
L1:		.word LIT,'"',PARSE,NEWSTRING,DUPTOR,PLACE,RFROM,COUNT
	.word PTHENP
L2:	.word UNNEST

	.deferword "COMPILE,",COMPILECOMMA,APPCOMPILECOMMA

	.immeddeferword "(",PAREN,UNDERPAREN

	.immedcolonword "\",BACKSLASH
	.word SOURCE,TOIN,STORE,DROP,UNNEST

	.colonword "?STACK",QSTACK
	.word DEPTH,ZLESS,THROWSTACKUNDER,QTHROW
	.word UNNEST

	.colonword "(NUMBER,)",PNUMBERCOMMAP
PNUMBERCOMMAPx ; this is just to get rid of label redefinitions for local labels
	.word DOUBLEQ,ZEQUAL
	.word QBRANCH,_L1
		.word DROP
	.word PTHENP
_L1:.word STATE,FETCH
	.word QBRANCH,_L2
		.word DOUBLEQ
		.word QBRANCH,_L3
			.word SWAP,LITERAL
		.word PTHENP
_L3:	.word LITERAL
	.word PTHENP
_L2:.word UNNEST

	.deferword "NUMBER,",NUMBERCOMMA,PNUMBERCOMMAP

	.colonword "([)",PLBRACKETP
	.word STATE,OFF
	.word UNNEST

	.immeddeferword "[",LBRACKET,PLBRACKETP

	.colonword "(])",PRBRACKETP
	.word STATE,ON
	.word UNNEST

	.deferword "]",RBRACKET,PRBRACKETP

	.deferword "ALIGN",ALIGN,NOOP

	.colonword "(ALLOT)",PALLOTP
	.word DP,PLUSSTORE
	.word UNNEST

	.deferword "ALLOT",ALLOT,PALLOTP

	; ( n -- )
	.colonword "?MEMCHK",QMEMCHK
	.word ZMAX,HERE,PLUS,TOPOFMEM,ONEMINUS,UGREATER
	.word THROWOUTOFMEM,QTHROW,UNNEST

	.varword "LAST",LAST
	.word COLDNFA

	.varword "DEFER-LIST",DEFERLIST
	.word PREVDEFER

	.colonword "(HIDE)",PHIDEP
	.word LAST,FETCH,NTOLINK,FETCH,CURRENT,FETCH,STORE
	.word UNNEST

	.deferword "HIDE",HIDE,PHIDEP

	.colonword "(REVEAL)",PREVEALP
	.word LAST,FETCH,CURRENT,FETCH,STORE
	.word UNNEST

	.deferword "REVEAL",REVEAL,PREVEALP

	.colonword '"NAME,',QUOTENAMECOMMA
	.word ALIGN,MAXNAMECHARS,MIN,DUP,ZEQUAL,THROWNAMEREQD,QTHROW
	.word CAPS,FETCH
	.word QBRANCH,+
	.word	TWODUP,UPPER
	.word PTHENP
+	.word HERE,TOR,DUP,CCOMMA,HERE,SWAP,DUP,ALLOT,MOVE,ALIGN
	.word RFETCH,COMMA,CURRENT,FETCH,FETCH,COMMA
	.word RFROM,LAST,STORE
	.word UNNEST

	.colonword '("HEADER)',PQUOTEHEADERP
	.word QUOTENAMECOMMA,LAST,FETCH,CURRENT,FETCH,STORE
	.word UNNEST

	.deferword '"HEADER',QUOTEHEADER,PQUOTEHEADERP

	.colonword "(HEADER)",PHEADERP
	.word LIT,100,QMEMCHK,BL,DOWORD,COUNT,QUOTEHEADER
	.word UNNEST

	.deferword "HEADER",HEADER,PHEADERP

	.varword "CSP",CSP
	.word 0

	.colonword "!CSP",STORECSP
	.word SPFETCH,CSP,STORE
	.word UNNEST

	.colonword "?CSP",QCSP
	.word SPFETCH,CSP,FETCH,DOXOR,THROWSTACKCHG,QTHROW
	.word UNNEST

	.colonword "(:)",PCOLONP
	.word HEADER,HIDE,DOCOLONCON,COMMA,STORECSP,RBRACKET
	.word UNNEST

	.deferword ":",COLON,PCOLONP

	.colonword "?COMP",QCOMP
	.word STATE,FETCH,ZEQUAL,THROWCOMPONLY,QTHROW
	.word UNNEST

	.colonword "?EXEC",QEXEC
	.word STATE,FETCH,THROWEXECONLY,QTHROW
	.word UNNEST

	.deferword "DO-;CHAIN",DOSEMICHAIN,NOOP

	.immedcolonword ";",SEMICOLON
	.word QCOMP,QCSP,REVEAL
	.word COMPILE,UNNEST
	.word LBRACKET,DOSEMICHAIN
	.word UNNEST

	.colonword "CREATE",CREATE
	.word HEADER,LIT,DOVAR,COMMA
	.word UNNEST

	.codeword "@(IP)",FETCHPIPP
	ldy #2
	lda (RP),y
	sta GP0
	iny
	lda (RP),y
	sta GP0+1
	ldy #1
	lda (GP0),y
	tax
	lda (GP0)
	jsr dpush
	clc
	lda GP0
	adc #2
	ldy #2
	sta (RP),y
	iny
	lda GP0+1
	adc #0
	sta (RP),y
	jmp NEXT

; : VARIABLE  CREATE 0 , ;
	.colonword "VARIABLE",VARIABLE
	.word CREATE,ZERO,COMMA
	.word UNNEST

; : CONSTANT  HEADER DOCON , , ;
	.colonword "CONSTANT",CONSTANT
	.word HEADER,DOCONCON,COMMA,COMMA
	.word UNNEST

; 128 CONSTANT IMMEDBIT
	.constword "IMMEDBIT",IMMEDBIT,128

; : IMMEDIATE IMMEDBIT LAST @ TOGGLE ;
	.colonword "IMMEDIATE",IMMEDIATE
	.word IMMEDBIT,LAST,FETCH,TOGGLE
	.word UNNEST

; : ?PAIRS    ( n1 n2 -- )    XOR THROW_MISMATCH ?THROW ;
	.colonword "?PAIRS",QPAIRS
	.word DOXOR,THROWMISMATCH,QTHROW
	.word UNNEST

; : >MARK     ( -- addr )     HERE 0 , ;
	.colonword ">MARK",FWDMARK
	.word HERE,ZERO,COMMA
	.word UNNEST

; : >RESOLVE  ( addr -- )     HERE SWAP ! ;
	.colonword ">RESOLVE",FWDRESOLVE
	.word HERE,SWAP,STORE
	.word UNNEST

; : <MARK     ( -- addr )     HERE ;
	.colonword "<MARK",BACKMARK
	.word HERE
	.word UNNEST

; : <RESOLVE  ( addr -- )     , ;
	.colonword "<RESOLVE",BACKRESOLVE
	.word COMMA
	.word UNNEST

; : AHEAD     ?COMP COMPILE BRANCH >MARK 2 ; IMMEDIATE
	.immedcolonword "AHEAD",AHEAD
	.word QCOMP,COMPILE,BRANCH,FWDMARK,TWO
	.word UNNEST

; : IF        ?COMP COMPILE ?BRANCH >MARK 2 ; IMMEDIATE
	.immedcolonword "IF",IF
	.word QCOMP,COMPILE,QBRANCH,FWDMARK,TWO
	.word UNNEST

; : THEN      ?COMP 2 ?PAIRS COMPILE (THEN) >RESOLVE ; IMMEDIATE
	.immedcolonword "THEN",THEN
	.word QCOMP,TWO,QPAIRS,COMPILE,PTHENP,FWDRESOLVE
	.word UNNEST

; : ENDIF     ?COMP 2 ?PAIRS COMPILE (THEN) >RESOLVE ; IMMEDIATE
	.immedcolonword "ENDIF",ENDIF
	.word QCOMP,TWO,QPAIRS,COMPILE,PTHENP,FWDRESOLVE
	.word UNNEST

; : ELSE      ?COMP 2 ?PAIRS COMPILE BRANCH >MARK SWAP >RESOLVE 2 ; IMMEDIATE
	.immedcolonword "ELSE",ELSE
	.word QCOMP,TWO,QPAIRS,COMPILE,BRANCH,FWDMARK,SWAP,FWDRESOLVE,TWO
	.word UNNEST

; : BEGIN     ?COMP COMPILE  (BEGIN) <MARK 1 ; IMMEDIATE
	.immedcolonword "BEGIN",BEGIN
	.word QCOMP,COMPILE,PBEGINP,BACKMARK,ONE
	.word UNNEST

; : UNTIL     ?COMP 1 ?PAIRS COMPILE (UNTIL) <RESOLVE ; IMMEDIATE
	.immedcolonword "UNTIL",UNTIL
	.word QCOMP,ONE,QPAIRS,COMPILE,PUNTILP,BACKRESOLVE
	.word UNNEST

; : WHILE     ?COMP COMPILE  (WHILE) >MARK 2 2SWAP ; IMMEDIATE
	.immedcolonword "WHILE",WHILE
	.word QCOMP,COMPILE,PWHILEP,FWDMARK,TWO,TWOSWAP
	.word UNNEST

; : REPEAT    ?COMP 1 ?PAIRS COMPILE (REPEAT) <RESOLVE 2 ?PAIRS >RESOLVE ; IMMEDIATE
	.immedcolonword "REPEAT",REPEAT
	.word QCOMP,ONE,QPAIRS,COMPILE,PREPEATP,BACKRESOLVE,TWO,QPAIRS,FWDRESOLVE
	.word UNNEST

; : DO        ?COMP COMPILE (DO) >MARK 3 ; IMMEDIATE
	.immedcolonword "DO",DO
	.word QCOMP,COMPILE,PDOP,FWDMARK,LIT,3
	.word UNNEST

; : LOOP      ?COMP 3 ?PAIRS COMPILE (LOOP) DUP CELL+ <RESOLVE >RESOLVE ; IMMEDIATE
	.immedcolonword "LOOP",LOOP
	.word QCOMP,LIT,3,QPAIRS,COMPILE,PLOOPP,DUP,CELLPLUS,BACKRESOLVE,FWDRESOLVE
	.word UNNEST

; : +LOOP     ?COMP 3 ?PAIRS COMPILE (+LOOP) DUP CELL+ <RESOLVE >RESOLVE ; IMMEDIATE
	.immedcolonword "+LOOP",PLUSLOOP
	.word QCOMP,LIT,3,QPAIRS,COMPILE,PPLUSLOOPP,DUP,CELLPLUS,BACKRESOLVE,FWDRESOLVE
	.word UNNEST

; : EXIT      ( -- )
;            ?COMP COMPILE (EXIT) ; IMMEDIATE
;            \ PARMS
;            \ IF      COMPILE (EXITP)
;            \ ELSE    COMPILE (EXIT)
;            \ THEN    ; IMMEDIATE
	.immedcolonword "EXIT",EXIT
	.word QCOMP,COMPILE,PEXITP
	.word UNNEST

; : RECURSE   ( -- )
;             ?COMP LAST @ NAME> COMPILE, ; IMMEDIATE
	.immedcolonword "RECURSE",RECURSE
	.word QCOMP,LAST,FETCH,NAMETO,COMPILECOMMA
	.word UNNEST

; : >IS       ( xt -- addr )      CELL+ ;
	.colonword ">IS",TOIS
	.word CELLPLUS
	.word UNNEST

; : (IS)      ( xt -- )           @(IP) >IS ! ;
	.colonword "(IS)",PISP
	.word FETCHPIPP,TOIS,STORE
	.word UNNEST

; : ?IS       ( xt -- xt )        DUP @ DODEFER <> THROW_NOTDEFER ?THROW ;
	.colonword "?IS",QIS
	.word DUP,FETCH,DODEFERCON,NOTEQUAL,THROWNOTDEFER,QTHROW
	.word UNNEST

; : IS        ( xt -<name>- )
;             STATE @
;             IF      COMPILE (IS) ' ?IS ,
;             ELSE    ' ?IS >IS !
;             THEN    ; IMMEDIATE
	.immedcolonword "IS",IS
	.word STATE,FETCH
	.word QBRANCH,+
	.word	COMPILE,PISP,TICK,QIS,COMMA
	.word BRANCH,++
+	.word	TICK,QIS,TOIS,STORE
	.word PTHENP
+	.word UNNEST

; : CALL,     ( addr -- ) \ EA 20 ==> NOP JSR
;             ( $20EA ) 8426 , , ;
	.colonword "CALL,",CALLCOMMA
	.word LIT,$20EA,COMMA,COMMA
	.word UNNEST

; : (;CODE)   R> LAST @ NAME> ! ;
	.colonword "(;CODE)",PSEMICODEP
	.word RFROM,LAST,FETCH,NAMETO,STORE
	.word UNNEST

; : DOES>     ?COMP COMPILE (;CODE) DODOES CALL, ; IMMEDIATE
	.immedcolonword "DOES>",DOES
	.word QCOMP,COMPILE,PSEMICODEP,DODOESCON,CALLCOMMA
	.word UNNEST

; : POSTPONE  DEFINED DUP 0= ?MISSING 0<
;             IF COMPILE COMPILE THEN
;             COMPILE, ; IMMEDIATE
	.immedcolonword "POSTPONE",POSTPONE
	.word DEFINED,DUP,ZEQUAL,QMISSING,ZLESS
	.word QBRANCH,+
	.word	COMPILE,COMPILE
	.word PTHENP
+	.word COMPILECOMMA
	.word UNNEST

; : [']       ' POSTPONE LITERAL ; IMMEDIATE
	.immedcolonword "[']",BTICKB
	.word TICK,LITERAL
	.word UNNEST

; : CHAR      BL WORD 1+ C@ ;
	.colonword "CHAR",DOCHAR
	.word BL,DOWORD,ONEPLUS,CFETCH
	.word UNNEST

; : [CHAR]    CHAR POSTPONE LITERAL ; IMMEDIATE
	.immedcolonword "[CHAR]",BCHARB
	.word DOCHAR,LITERAL
	.word UNNEST

; : ."        ( -- )
;             COMPILE (.") ," ; IMMEDIATE
	.immedcolonword '."',DOTQUOTE
	.word COMPILE,PDOTQUOTEP,COMMAQUOTE
	.word UNNEST

; : DEFER     HEADER DODEFER , COMPILE NOOP DEFER-LIST LINK, COMPILE NOOP ;
	.colonword "DEFER",DEFER
	.word HEADER,DODEFERCON,COMMA,COMPILE,NOOP,DEFERLIST,LINKCOMMA,COMPILE,NOOP
	.word UNNEST

; : (ABORT")      ( f -- )
;                 ((")) SWAP
;                 IF      MSG ! THROW_ABORT? THROW
;                 THEN    DROP ;
	.colonword '(ABORT")',PABORTQP
	.word PPQUOTEPP,SWAP
	.word QBRANCH,+
	.word	MSG,STORE,THROWABORTQ,THROW
	.word PTHENP
+	.word DROP
	.word UNNEST

; : ABORT         ( -- )
;                 THROW_ABORT THROW ;
	.colonword "ABORT",ABORT
	.word THROWABORT,THROW
	.word UNNEST

; : ABORT"        ( -- )
;                 COMPILE (ABORT") ," ; IMMEDIATE
	.immedcolonword 'ABORT"',ABORTQ
	.word COMPILE,PABORTQP,COMMAQUOTE
	.word UNNEST

; : ?TO_CHECK ( xt -- xtbody )
;             DUP @ >R
;             >BODY DUP CELL+ @ -1 =
;             R@ DOCON = OR
;             R@ DOCOLON = OR
;             R@ DOVAR = OR
;             R> DODEFER = OR
;             THROW_NOTVALUE ?THROW ;
	.colonword "?TO_CHECK",QTOCHECK
	.word DUP,FETCH,TOR,TOBODY,DUP,CELLPLUS,FETCH,LIT,$FFFF,EQUAL
	.word RFETCH,DOCONCON,EQUAL,DOOR
	.word RFETCH,DOCOLONCON,EQUAL,DOOR
	.word RFETCH,DOVARCON,EQUAL,DOOR
	.word RFROM,DODEFERCON,EQUAL,DOOR
	.word THROWNOTVALUE,QTHROW
	.word UNNEST

; : TOCOMPEXEC    ( -- )
;             ' ?TO_CHECK + STATE @ IF COMPILE, ELSE EXECUTE THEN ;
	.colonword "TOCOMPEXEC",TOCOMPEXEC
	.word TICK,QTOCHECK,PLUS,STATE,FETCH
	.word QBRANCH,+
	.word	COMPILECOMMA
	.word BRANCH,++
+	.word	EXECUTE
	.word PTHENP
+	.word UNNEST

; : TO        ( n -<value>- )
;             CELL TOCOMPEXEC ; IMMEDIATE
	.immedcolonword "TO",TO
	.word CELL,TOCOMPEXEC
	.word UNNEST

; : +TO       ( n -<value>- )
;             [ 2 CELLS ] LITERAL TOCOMPEXEC ; IMMEDIATE
	.immedcolonword "+TO",PLUSTO
	.word TWO,CELLS,TOCOMPEXEC
	.word UNNEST

; DEFER ALIGNED ' NOOP IS ALIGNED
	.deferword "ALIGNED",ALIGNED,NOOP

; : _SAVE-INPUT   ( -- xn .. x1 n )
;                 >IN @ SOURCE-POSITION SOURCE-ID SOURCE 5 ;
	.colonword "_SAVE-INPUT",USAVEINPUT
	.word TOIN,FETCH,SOURCEPOSITION,SOURCEID,SOURCE,LIT,5
	.word UNNEST

; : _RESTORE-INPUT    ( xn .. x1 n -- f )
;                 5 ?PAIRS
;                 (SOURCE) 2! TO SOURCE-ID TO SOURCE-POSITION >IN ! 0 ;
	.colonword "_RESTORE-INPUT",URESTOREINPUT
	.word LIT,5,QPAIRS
	.word PSOURCEP,TWOSTORE,SOURCEID+4,SOURCEPOSITION+4,TOIN,STORE,ZERO
	.word UNNEST

; DEFER SAVE-INPUT    ' _SAVE-INPUT IS SAVE-INPUT
	.deferword "SAVE-INPUT",SAVEINPUT,USAVEINPUT

; DEFER RESTORE-INPUT ' _RESTORE-INPUT IS RESTORE-INPUT
	.deferword "RESTORE-INPUT",RESTOREINPUT,URESTOREINPUT

; : EVALUATE      ( addr len -- )
;                 SAVE-INPUT (SAVE-INPUT)
;                 (SOURCE) 2! >IN OFF -1 TO SOURCE-ID 0 TO SOURCE-POSITION
;                 ['] INTERPRET CATCH DUP
;                 IF      >IN @ SWAP
;                 THEN    (RESTORE-INPUT) RESTORE-INPUT DROP DUP
;                 IF      SWAP >IN !
;                 THEN    THROW ;
	.colonword "EVALUATE",EVALUATE
	.word SAVEINPUT,PSAVEINPUTP
	.word PSOURCEP,TWOSTORE,TOIN,OFF,LIT,$FFFF,SOURCEID+4,ZERO,SOURCEPOSITION+4
	.word LIT,INTERPRET,CATCH,DUP
	.word QBRANCH,+
	.word	TOIN,FETCH,SWAP
	.word PTHENP
+	.word PRESTOREINPUTP,RESTOREINPUT,DROP,DUP
	.word QBRANCH,+
	.word	SWAP,TOIN,STORE
	.word PTHENP
+	.word THROW
	.word UNNEST

;-----------------------------------------------------------------------------
; CORE Extension Words
;-----------------------------------------------------------------------------

; : :NONAME   ( -- xt )
;             ALIGN HERE DOCOLON , !CSP ] ;
	.colonword ":NONAME",NONAME
	.word ALIGN,HERE,DOCOLONCON,COMMA,STORECSP,RBRACKET
	.word UNNEST

; : ?DO       ?COMP COMPILE (?DO) >MARK 3 ; IMMEDIATE
	.immedcolonword "?DO",QDO
	.word QCOMP,COMPILE,PQDOP,FWDMARK,LIT,3
	.word UNNEST

; : AGAIN     ?COMP 1 ?PAIRS COMPILE (AGAIN) <RESOLVE ; IMMEDIATE
	.immedcolonword "AGAIN",AGAIN
	.word QCOMP,ONE,QPAIRS,COMPILE,PAGAINP,BACKRESOLVE
	.word UNNEST

; : CASE      ?COMP COMPILE (CASE) 0 ; IMMEDIATE
	.immedcolonword "CASE",CASE
	.word QCOMP,COMPILE,PCASEP,ZERO
	.word UNNEST

; : OF        ?COMP COMPILE (OF) >MARK 4 ; IMMEDIATE
	.immedcolonword "OF",OF
	.word QCOMP,COMPILE,POFP,FWDMARK,LIT,4
	.word UNNEST

; : ENDOF     ?COMP 4 ?PAIRS COMPILE (ENDOF) >MARK SWAP >RESOLVE 5 ; IMMEDIATE
	.immedcolonword "ENDOF",ENDOF
	.word QCOMP,LIT,4,QPAIRS,COMPILE,PENDOFP,FWDMARK,SWAP,FWDRESOLVE
	.word LIT,5
	.word UNNEST

; : ENDCASE   ?COMP COMPILE (ENDCASE)
;             BEGIN ?DUP WHILE 5 ?PAIRS >RESOLVE REPEAT ; IMMEDIATE
	.immedcolonword "ENDCASE",ENDCASE
	.word QCOMP,COMPILE,PENDCASEP
	.word PBEGINP
-	.word	QDUP
	.word PWHILEP,+
	.word	LIT,5,QPAIRS,FWDRESOLVE
	.word PREPEATP,-
+	.word UNNEST

; : ERASE     ( addr len -- ) 0 FILL ;
	.colonword "ERASE",ERASE
	.word ZERO,FILL
	.word UNNEST

; : VALUE     ( n -<name>- )
;             HEADER DOVALUE , , DOVALUE! , DOVALUE+! , ;
	.colonword "VALUE",VALUE
	.word HEADER,DOVALUECON,COMMA,COMMA,DOVALUESTORECON,COMMA,DOVALUEPSTORECON,COMMA
	.word UNNEST

; : [COMPILE] ' COMPILE, ; IMMEDIATE
	.immedcolonword "[COMPILE]",BCOMPILEB
	.word TICK,COMPILECOMMA
	.word UNNEST

; : ?EXIT     ( f -- )
;             ?COMP COMPILE ?BRANCH >MARK
;             [COMPILE] EXIT COMPILE (THEN) >RESOLVE ; IMMEDIATE
	.immedcolonword "?EXIT",QEXIT
	.word QCOMP,COMPILE,QBRANCH,FWDMARK
	.word EXIT,COMPILE,PTHENP,FWDRESOLVE
	.word UNNEST

; : D.R       ( d n -- )          >R (D.) R> OVER - SPACES TYPE ;
	.colonword "D.R",DDOTR
	.word TOR,PDDOTP,RFROM,OVER,MINUS,SPACES,DOTYPE
	.word UNNEST

; : .R        ( n1 n2 -- )        >R S>D R> D.R ;
	.colonword ".R",DOTR
	.word TOR,STOD,RFROM,DDOTR
	.word UNNEST

; : U.R       ( u n -- )          0 SWAP D.R ;
	.colonword "U.R",UDOTR
	.word ZERO,SWAP,DDOTR
	.word UNNEST

; : H.        ( u -- )            BASE @ SWAP HEX U. BASE ! ;
	.colonword "H.",HDOT
	.word BASE,FETCH,SWAP,HEX,UDOT,BASE,STORE
	.word UNNEST

; : ?         ( addr -- )         @ . ;
	.colonword "?",QUESTION
	.word FETCH,DOT
	.word UNNEST

; : H.R       ( u n -- )
;             BASE @ >R HEX
;             >R 0 <# #S #> R> OVER - SPACES TYPE
;             R> BASE ! ;
	.colonword "H.R",HDOTR
	.word BASE,FETCH,TOR,HEX,TOR,ZERO,BEGNUMBER,NUMBERSIGNS,NUMBEREND
	.word RFROM,OVER,MINUS,SPACES,DOTYPE,RFROM,BASE,STORE
	.word UNNEST

; : H.N       ( u n -- )
;             BASE @ >R HEX
;             >R 0 <# R> 0 ?DO # LOOP #> TYPE
;             R> BASE ! ;
	.colonword "H.N",HDOTN
	.word BASE,FETCH,TOR,HEX
	.word TOR,ZERO,BEGNUMBER,RFROM,ZERO
	.word PQDOP,+
-	.word	NUMBERSIGN
	.word PLOOPP,-
+	.word NUMBEREND,DOTYPE,RFROM,BASE,STORE
	.word UNNEST

; : H.2       ( u -- )            2 H.N ;
	.colonword "H.2",HDOTTWO
	.word TWO,HDOTN
	.word UNNEST

; : H.4       ( u -- )            4 H.N ;
	.colonword "H.4",HDOTFOUR
	.word LIT,4,HDOTN
	.word UNNEST

; : H.8       ( u -- )            8 H.N ;
	.colonword "H.8",HDOTEIGHT
	.word LIT,8,HDOTN
	.word UNNEST

; : 0<>       ( n -- f )          0= INVERT ;
	.colonword "0<>",ZNOTEQUAL
	.word ZEQUAL,INVERT
	.word UNNEST

; : HOLDS             ( addr u -- )
;     HLD @ OVER - DUP HLD ! SWAP MOVE ;
	.colonword "HOLDS",HOLDS
	.word HLD,FETCH,OVER,MINUS,DUP,HLD,STORE,SWAP,MOVE
	.word UNNEST

; : PARSE-NAME        ( -<name>- -- addr u )
;     BL WORD COUNT ;
	.colonword "PARSE-NAME",PARSENAME
	.word BL,DOWORD,COUNT
	.word UNNEST

; : BUFFER:           ( n -<name>- )
;     CREATE ALLOT ;
	.colonword "BUFFER:",BUFFERCOLON
	.word CREATE,ALLOT
	.word UNNEST

; : DEFER!            ( xt2 xt1 -- )
;     ?IS >IS ! ;
	.colonword "DEFER!",DEFERSTORE
	.word QIS,TOIS,STORE
	.word UNNEST

; : DEFER@            ( xt1 -- xt2 )
;     ?IS >IS @ ;
	.colonword "DEFER@",DEFETFETCH
	.word QIS,TOIS,FETCH
	.word UNNEST

; : ACTION-OF         ( -<name>- -- xt )
;     ' ?IS >IS
;     STATE @
;     IF      POSTPONE LITERAL POSTPONE @
;     ELSE    @
;     THEN    ; IMMEDIATE
	.immedcolonword "ACTION-OF",ACTIONOF
	.word TICK,QIS,TOIS,STATE,FETCH
	.word QBRANCH,+
	.word	LITERAL,COMPILE,FETCH
	.word BRANCH,++
+	.word	FETCH
	.word PTHENP
+	.word UNNEST

; CREATE TEMP$ MAXSTRING 1+ ALLOT
	.varword "TEMP$",TEMPSTR
	.fill MAXBUFSIZE+1

; DEFER NEW$ ' TEMP$ IS NEW$
	.deferword "NEW$",NEWSTR,TEMPSTR

; : LOCALALLOC
;     POSTPONE (LOCALALLOC) ; IMMEDIATE
	.immedcolonword "LOCALALLOC",LOCALALLOC
	.word COMPILE,PLOCALALLOCP
	.word UNNEST

; : C+PLACE   ( c dest -- ) \ append character to counted string at dest
;             DUP CINCR COUNT + 1- C! ;
	.colonword "C+PLACE",CPLUSPLACE
	.word DUP,CINCR,COUNT,PLUS,ONEMINUS,CSTORE
	.word UNNEST

; : (C")          ( -- cstr )
;                 ((")) ;
	.colonword '(C")',PCQUOTEP
	.word PPQUOTEPP
	.word UNNEST

; : C"        ( -- )
;             STATE @
;             IF      COMPILE (C") ,"
;             ELSE    [CHAR] " WORD NEW$ DUP>R OVER C@ 1+ MOVE R>
;             THEN    ; IMMEDIATE
	.immedcolonword 'C"',CQUOTE
	.word STATE,FETCH
	.word QBRANCH,+
	.word	COMPILE,PCQUOTEP,COMMAQUOTE
	.word BRANCH,++
+	.word	LIT,'"',DOWORD,NEWSTR,DUPTOR,OVER,CFETCH,ONEPLUS,MOVE,RFROM
	.word PTHENP
+	.word UNNEST

; UNUSED    ( -- u )
;           TOP-OF-MEM DP @ - ;
	.colonword "UNUSED",UNUSED
	.word TOPOFMEM,DP,FETCH,MINUS
	.word UNNEST

;*****************************************************************************
; Forth Interpreter
;*****************************************************************************

	.varword "STATE",STATE
	.word 0

	.deferword "SAVE-SRC",SAVESRC,NOOP
	.deferword "?UNSAVE-SRC",QUNSAVESRC,NOOP

	.colonword "(INTERPRET)",PINTERPRETP
PINTERPRETPx ; this is just to get rid of label redefinitions for local labels
	.word PBEGINP
_L1		.word BL,DOWORD,DUP,CFETCH
	.word PWHILEP,_L2
		.word SAVESRC,FIND,QDUP
		.word QBRANCH,_L3
			.word STATE,FETCH,EQUAL
			.word QBRANCH,_L4
				.word COMPILECOMMA
			.word BRANCH,_L5
_L4				.word EXECUTE,QSTACK
			.word PTHENP
_L5		.word BRANCH,_L6
_L3			.word NUMBER,NUMBERCOMMA
		.word PTHENP
_L6		.word QUNSAVESRC
	.word PREPEATP,_L1
_L2	.word DROP,UNNEST

	.deferword "INTERPRET",INTERPRET,PINTERPRETP

;*****************************************************************************
; Main Forth Interpreter Loop
;*****************************************************************************

	.varword "?LOADING",QLOADING
	.word 0

	.varword "LOADLINE",LOADLINE
	.word 0

	.varword ".SMAX",DOTSMAX
	.word 8

	.colonword "(.PROMPT)",PDOTPROMPTP
	.word CR,PDOTQUOTEP
	.ptext "> "
	.word UNNEST

	.colonword "(.OK)",PDOTOKP
PDOTOKPx
	.word PDOTQUOTEP
	.ptext " ok"
	.word DEPTH,DOTSMAX,FETCH,MIN,ZERO
	.word PQDOP,_L1
_L2:	.word	LIT,'.',EMIT
	.word PLOOPP,_L2
_L1:.word UNNEST

	.deferword ".OK",DOTOK,PDOTOKP
	.deferword ".PROMPT",DOTPROMPT,PDOTPROMPTP
	.deferword ".NOTOK",DOTNOTOK,NOOP

	.varword "(EXCEPTION-SOURCE)",PEXCEPTIONSOURCEP
	.word 0
	.word 0

	.colonword "?TYPE",QTYPE
QTYPEx
	.word COUNT,QDUP
	.word QBRANCH,_L1
	.word	DOTYPE,SPACE
	.word BRANCH,_L2
_L1:.word	DROP
	.word PTHENP
_L2:.word UNNEST

	.colonword "(TYPEMSG)",PTYPEMSGP
PTYPEMSGPx
	.word QLOADING,FETCH
	.word QBRANCH,_L1
		.word PEXCEPTIONSOURCEP
	.word BRANCH,_L2
_L1:	.word PSOURCEP
	.word PTHENP
_L2:.word CR,TWOFETCH,DOTYPE,CR,TOIN,FETCH,DUP,QLOADING,FETCH
	.word QBRANCH,_L3
		.word PEXCEPTIONSOURCEP
	.word BRANCH,_L4
_L3:	.word PSOURCEP
	.word PTHENP
_L4:.word FETCH,LESS,PLUS,POCKET,CFETCH,DUPTOR,MINUS,ONEMINUS,SPACES
	.word RFROM,ONE,MAX,ZERO
	.word PQDOP,_L5
_L6:	.word LIT,'^',EMIT
	.word PLOOPP,_L6
_L5:	.word BASE,FETCH,TOR,DECIMAL,CR,DOTYPE
	.word PDOTQUOTEP
	.ptext '('
	.word DUP,LASTERROR+4,STOD,PDDOTP,DOTYPE
	.word PDOTQUOTEP
	.ptext '): '
	.word POCKET,QTYPE,THROWMSGS
	.word PBEGINP
_L7:	.word FETCH,QDUP
	.word PWHILEP,_L8
		.word DUP,CELLPLUS,FETCH,LASTERROR,EQUAL
		.word QBRANCH,_L9
			.word TWO,CELLSPLUS,QTYPE,NULLMSG,MSG,STORE,PTRNULL
		.word PTHENP
_L9:.word PREPEATP,_L7
_L8:.word MSG,FETCH,QTYPE,NULLMSG,MSG,STORE,QLOADING,FETCH
	.word QBRANCH,_LA
		.word PDOTQUOTEP
		.ptext 'in file '
		.word LOADFILE,FETCH,CELLPLUS,QTYPE
		.word PDOTQUOTEP
		.ptext 'at line '
		.word LOADLINE,FETCH,DOT
	.word PTHENP
_LA:.word RFROM,BASE,STORE,UNNEST

	.deferword "EDIT-ERROR",EDITERROR,NOOP

	.colonword "(MESSAGE)",PMESSAGEP
PMESSAGEPx
	.word DUP,ONEPLUS
	.word QBRANCH,_L1
	.word 	PSQUOTEP
	.ptext 	"Error "
	.word 	PTYPEMSGP
	.word	QLOADING
	.word	QBRANCH,_L3
	.word		EDITERROR
	.word	PTHENP
_L3:.word BRANCH,_L2
_L1:.word 	DROP
	.word PTHENP
_L2:.word UNNEST

	.deferword "MESSAGE",MESSAGE,PMESSAGEP

	.colonword "(RESET-STACKS)",PRESETSTACKSP
	.word SP0,FETCH,SPSTORE
	.word UNNEST

	.deferword "RESET-STACKS",RESETSTACKS,PRESETSTACKSP

	.colonword "QUERY-INTERPRET",QUERYINTERPRET
	.word QUERY,SPACE,INTERPRET
	.word UNNEST

	.colonword "(QUIT)",PQUITP
PQUITPx
	.word RP0,FETCH,RPSTORE
_L1:.word PBEGINP
	.word	LBRACKET,QLOADING,OFF
	.word	PBEGINP
_L2:.word		DOTPROMPT,LIT,QUERYINTERPRET,CATCH,QDUP,ZEQUAL
	.word	PWHILEP,_L3
	.word		STATE,FETCH,ZEQUAL
	.word		QBRANCH,_L4
	.word			DOTOK
	.word		PTHENP
_L4:.word	PREPEATP,_L2
_L3:.word 	DUP,ONEPLUS
	.word	QBRANCH,_L5
	.word		MESSAGE
	.word	PTHENP
_L5:.word	DOTNOTOK,RESETSTACKS
	.word PAGAINP,_L1
	.word UNNEST

	.deferword "QUIT",QUIT,PQUITP

;*****************************************************************************
; Forth File I/O Words
;*****************************************************************************

	.constword "R/O",READONLY,FILEIO_MODE_READ
	.constword "R/W",READWRITE,FILEIO_MODE_RW
	.constword "W/O",WRITEONLY,FILEIO_MODE_WRITE

	.deferword "BIN",BIN,NOOP

	.codeword "OPEN-FILE",OPENFILE
	jsr dpop
	sta FILEIO_CMODE
	jsr dpopToGP2
	jsr dpopToGP0
	lda #<FILEIO_FILENAME
	sta GP1
	lda #>FILEIO_FILENAME
	sta GP1+1
	clc
	lda GP1
	adc GP2
	sta GP3
	lda GP1+1
	adc GP2+1
	sta GP3+1
	lda #0
	sta (GP3)
	jsr memMove
	lda #FILEIO_CMD_OPEN
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CFD
	ldx #0
	jsr dpush
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.deferword "CREATE-FILE",CREATEFILE,OPENFILE

	.codeword "CLOSE-FILE",CLOSEFILE
	jsr dpop
	sta FILEIO_CFD
	lda FILEIO_CMD_CLOSE
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.codeword "READ-FILE",READFILE
	jsr dpop
	sta FILEIO_CFD
	jsr dpopToGP1
	jsr dpopToGP2
	stz GP0				; u2
	stz GP0+1
	lda #FILEIO_CMD_READ
	sta FILEIO_CCMD
-	lda GP1
	ora GP1+1
	beq +
	jsr doFileIO
	lda FILEIO_CSTATUS
	bne +
	lda FILEIO_CDATA_LO
	sta (GP2)
	jsr incGP2
	jsr decGP1
	jsr incGP0
	bra -
+	lda GP0
	ldx GP0+1
	jsr dpush
	lda FILEIO_CSTATUS
	cmp FILEIO_STATUS_EOF
	bne +
	lda #0
+	ldx #0
	jmp PUSHNEXT

	.codeword "READ-LINE",READLINE
	jsr dpop
	sta FILEIO_CFD
	jsr dpopToGP1
	jsr dpopToGP2
	stz GP0				; u2
	stz GP0+1
	lda #FILEIO_CMD_READ
	sta FILEIO_CCMD
-	lda GP1
	ora GP1+1
	beq +++
	jsr doFileIO
	lda FILEIO_CSTATUS
	bne +++
	lda FILEIO_CDATA_LO
	cmp #10
	beq +++
	sta (GP2)
	inc GP2
	bne +
	inc GP2+1
+	jsr decGP1
	inc GP0
	bne +
	inc GP0+1
+	bra -
+	pha
	lda GP0
	ldx GP0+1
	jsr dpush
	pla
	cpa #10
	beq ++
	lda FILEIO_CSTATUS
	cmp FILEIO_STATUS_ERR
	bne +
	lda #0				; io error
	tax
	jsr dpush
	lda FILEIO_CSTATUS
	bra ++++
+	lda GP0
	ora GP0+1
	bne +
	;lda #0				; eof
	tax
	jsr dpush
	bra ++
+	lda #$ff			; got a line
	tax
	jsr dpush
+	lda #0
+	tax
	jmp PUSHNEXT

	.codeword "WRITE-FILE",WRITEFILE
	jsr dpop
	sta FILEIO_CFD
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doWriteFile
	jmp PUSHNEXT

	.codeword "WRITE-LINE",WRITELINE
	jsr dpop
	sta FILEIO_CFD
	jsr dpopToGP2
	jsr dpopToGP1
	jsr doWriteFile
	cpa #0
	bne +
	lda #10
	sta FILEIO_CDATA_LO
	jsr doFileIO
	lda FILEIO_CSTATUS
	ldx #0
+	jmp PUSHNEXT

	.codeword "WAIT-FILE",WAITFILE
	jsr waitFileIO
	jmp NEXT

	.colonword "DELETE-FILE",DELETEFILE
	.word LIT,FILEIO_FILENAME,SWAP,TWODUP,TWOTOR
	.word MOVE,TWORFROM,PLUS,ZERO,SWAP,CSTORE
	.word LIT,FILEIO_CMD_DELETE,LIT,FILEIO_CCMD,CSTORE
	.word LIT,1,LIT,FILEIO_CREADY,CSTORE,WAITFILE
	.word LIT,FILEIO_CSTATUS,CFETCH
	.word UNNEST

	.codeword "FILE-POSITION",FILEPOSITION
	jsr dpop
	sta FILEIO_CFD
	lda #FILEIO_CMD_FILEPOS
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CDATA_LO
	ldx FILEIO_CDATA_HI
	jsr dpush
	lda FILEIO_CDATA_LO2
	ldx FILEIO_CDATA_HI2
	jsr dpush
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.codeword "FILE-SIZE",FILESIZE
	jsr dpop
	sta FILEIO_CFD
	lda #FILEIO_CMD_FILESIZ
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CDATA_LO
	ldx FILEIO_CDATA_HI
	jsr dpush
	lda FILEIO_CDATA_LO2
	ldx FILEIO_CDATA_HI2
	jsr dpush
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.codeword "REPOSITION-FILE",REPOSITIONFILE
	jsr dpop
	sta FILEIO_CFD
	jsr dpop
	sta FILEIO_CDATA_LO2
	stx FILEIO_CDATA_HI2
	jsr dpop
	sta FILEIO_CDATA_LO
	stx FILEIO_CDATA_HI
	lda #FILEIO_CMD_SEEK
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.codeword "RESIZE-FILE",RESIZEFILE
	jsr dpop
	sta FILEIO_CFD
	jsr dpop
	sta FILEIO_CDATA_LO2
	stx FILEIO_CDATA_HI2
	jsr dpop
	sta FILEIO_CDATA_LO
	stx FILEIO_CDATA_HI
	lda #FILEIO_CMD_RESIZE
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.codeword "FLUSH-FILE",FLUSHFILE
	jsr dpop
	sta FILEIO_CFD
	lda #FILEIO_CMD_FLUSH
	sta FILEIO_CCMD
	jsr doFileIO
	lda FILEIO_CSTATUS
	ldx #0
	jmp PUSHNEXT

	.varword "OPENBUF",OPENBUF
	.fill MAXBUFSIZE,0

	.varword "CUR-FILE",CURFILE
	.fill MAXBUFSIZE,0

	.varword "CUR-LINE",CURLINE
	.word 0

	.colonword "LINKFILE",LINKFILE
	.word QLOADING,FETCH
	.word QBRANCH,+
	.word	ALIGN,LOADFILE,LINKCOMMA
	.word	COUNT,HERE,PLACE,HERE,CFETCH,ONEPLUS,ALLOT
	.word BRANCH,++
+	.word	DROP
	.word PTHENP
+	.word UNNEST

	.varword "START-LINE",STARTLINE
	.word 0

	.colonword "?.REFILL",QDOTREFILL
	.word ECHO,FETCH
	.word QBRANCH,+
	.word	CR,SOURCE,DOTYPE
	.word PTHENP
+	.word UNNEST

	.deferword ".REFILL",DOTREFILL,QDOTREFILL

	.valueword "INCLUDING?",INCLUDINGQ,0

	.valueword "LEN-PREV",LENPREV,0

	.colonword "REFILL",REFILL
	.word SOURCEID,QDUP
	.word QBRANCH,++++
		.word ONEPLUS
		.word QBRANCH,+++
			.word ONE,LOADLINE,PLUSSTORE,TIB,DUP,MAXSTRING
			.word LENPREV,SOURCEPOSITION+6
			.word SOURCEID,READLINE,THROWFILEREADFAIL,QTHROW
			.word QBRANCH,+
				.word DUP,TWO,PLUS,LENPREV+4,TWODUP
				.word PEXCEPTIONSOURCEP,TWOSTORE,PSOURCEP,TWOSTORE
				.word TOIN,OFF,DOTREFILL,TRUECON,PEXITP
			.word BRANCH,++
+				.word ZERO,LENPREV+4
			.word PTHENP
+			.word TWODROP
		.word PTHENP
+		.word FALSECON,PEXITP
	.word PTHENP
+	.word CR,QUERY,TRUECON
	.word UNNEST

	.colonword ">LINE",TOLINE
	.word ONEMINUS,ZERO,MAX,QDUP
	.word QBRANCH,++
	.word	ZERO
	.word	PDOP,+
-	.word		REFILL,DROP
	.word	PLOOPP,-
+	.word PTHENP
+	.word UNNEST

	.deferword "STACK-CHECK",STACKCHECK,NOOP

	.colonword "DO-INCLUDE",DOINCLUDE
	.word INCLUDINGQ,TOR,TRUECON,INCLUDINGQ+4
	.word STARTLINE,FETCH,TOLINE,STARTLINE,OFF
	.word SOURCEPOSITION,TOR,ZERO,SOURCEPOSITION+4
	.word LENPREV,TOR,ZERO,LENPREV+4
	.word PBEGINP
-	.word	REFILL,INCLUDINGQ,DOAND
	.word PWHILEP,+
	.word	INTERPRET,STACKCHECK
	.word PREPEATP,-
+	.word RFROM,LENPREV+4,RFROM,SOURCEPOSITION+4,RFROM,INCLUDINGQ+4
	.word UNNEST

	.deferword "START-INCLUDE",STARTINCLUDE,NOOP

	.deferword "END-INCLUDE",ENDINCLUDE,NOOP

	.colonword "INCLUDE-FILE",INCLUDEFILE
	.word MAXSTRING,PLOCALALLOCP,TOR
	.word TIB,RFETCH,MAXSTRING,MOVE
	.word LOADFILE,FETCH,CELLPLUS,TOR,QLOADING,FETCH,TOR
	.word LOADLINE,FETCH,TOR,TOIN,FETCH,TOR,SOURCEID,TOR
	.word SOURCEID+4,SOURCE,TWOTOR,SOURCEPOSITION,TOR
	.word QLOADING,ON,POCKET,LINKFILE,LOADLINE,OFF
	.word ZERO,SOURCEPOSITION+4
	.word STARTINCLUDE
	.word LIT,DOINCLUDE,CATCH,SOURCEID,CLOSEFILE,DROP
	.word RFROM,SOURCEPOSITION+4,TWORFROM,PSOURCEP,TWOSTORE
	.word RFROM,SOURCEID+4
	.word ENDINCLUDE
	.word THROW
	.word RFROM,TOIN,STORE,RFROM,LOADLINE,STORE
	.word RFROM,QlOADING,STORE
	.word ALIGN,RFROM,LINKFILE
	.word RFROM,TIB,MAXSTRING,MOVE,QLOADING,FETCH
	.word QBRANCH,+
	.word	LOADFILE,FETCH,CELLPLUS,COUNT,CLIPSTRING,CURFILE,PLACE
	.word PTHENP
+	.word PLOCALFREEP,UNNEST

	; ( addr len -- fileid f )
	.colonword '("OPEN)',PQUOTEOPENP
	.word OPENBUF,PLACE
	.word OPENBUF,COUNT,READWRITE,OPENFILE,DUP,ZEQUAL
	.word QBRANCH,+
	.word	OPENBUF,COUNT,CURFILE,PLACE
	.word PTHENP
+	.word OPENBUF,COUNT,POCKET,PLACE,UNNEST

	.deferword '"OPEN',QUOTEOPEN,PQUOTEOPENP

	.colonword "INCLUDED",INCLUDED
	.word QUOTEOPEN,THROWFILENOTFOUND,QTHROW,INCLUDEFILE
	.word UNNEST

	.colonword "FLOAD",FLOAD
	.word BL,DOWORD,COUNT,INCLUDED
	.word UNNEST

	.varword "ECHO",ECHO
	.word 0

	.colonword "FORTH-KERNEL-SIZE",FORTHKERNELSIZE
	.word DP,FETCH,FORTHKERNELSTARTCON,MINUS
	.word UNNEST

	.colonword "FSAVE",FSAVE
	.word BL,DOWORD,COUNT
	.word WRITEONLY,OPENFILE,THROWFILECREATEFAIL,QTHROW,TOR
	.word FORTHKERNELSTARTCON,HERE,STORE,HERE,LIT,2
	.word RFETCH,WRITEFILE,THROWFILEWRITEFAIL,QTHROW
	.word FORTHKERNELSTARTCON,FORTHKERNELSIZE
	.word RFETCH,WRITEFILE,THROWFILEWRITEFAIL,QTHROW
	.word RFROM,CLOSEFILE,THROWFILECLOSEFAIL,QTHROW
	.word UNNEST

	.deferword "WELCOME",WELCOME,NOOP

	.colonword "COLD",COLD
	.word WELCOME,QUIT
	.word UNNEST

	COLDNFA = PREVHEADER

;-----------------------------------------------------------------------------
; Forth Kernel Cold Start
;-----------------------------------------------------------------------------

ICOLD:
	cld
	; initialize SP
	lda #<DSTACK_TOP
	sta SP
	sta SP0+2
	lda #>DSTACK_TOP
	sta SP+1
	sta SP0+3
	; initialize RP
	lda #<RSTACK_TOP
	sta RP
	sta RP0+2
	lda #>RSTACK_TOP
	sta RP+1
	sta RP0+3
	; initialize CFA
	lda #<COLD
	sta CFA
	lda #>COLD
	sta CFA+1
	; initialize BASE
	lda #10
	sta BASE+2
	stz BASE+3
	; and begin
	jmp EXEC

ENDOFKERNEL:
