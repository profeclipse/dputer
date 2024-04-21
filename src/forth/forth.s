    .setcpu "65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo

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

    .include "kernel.lbl"
    .include "kernel.inc"
    .include "forth.inc"
    .include "forthmac.inc"

    .include "zeropage.s"

    .segment "LOADADDR"

    .word KERNELSTART

    .segment "CODE"

KERNELSTART = *

ENTRY:
    jmp ICOLD

;*****************************************************************************
; Utility subroutines
;*****************************************************************************

;-----------------------------------------------------------------------------
; Forth stack push/pop
;-----------------------------------------------------------------------------

incSP:
    clc
    lda SP
    adc #2
    sta SP
    bcc @skip
    inc SP+1
@skip:
    rts

decSP:
    sec
    lda SP
    sbc #2
    sta SP
    bcs @skip
    dec SP+1
@skip:
    rts

dpush:
    sta (SP)
    txa
    ldy #1
    sta (SP),y
    jmp decSP

dpop:
    jsr incSP
    ldy #1
    lda (SP),y
    tax
    lda (SP)
    rts

dpopToGP0:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP0+1
    lda (SP)
    sta GP0
    rts

dpopToGP1:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP1+1
    lda (SP)
    sta GP1
    rts

dpopToGP2:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP2+1
    lda (SP)
    sta GP2
    rts

dpopToGP3:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP3+1
    lda (SP)
    sta GP3
    rts

dpopToGP4:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP4+1
    lda (SP)
    sta GP4
    rts

dpopToGP5:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP5+1
    lda (SP)
    sta GP5
    rts

dpopToGP6:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP6+1
    lda (SP)
    sta GP6
    rts

dpopToGP7:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP7+1
    lda (SP)
    sta GP7
    rts

dpopToGP8:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP8+1
    lda (SP)
    sta GP8
    rts

dpopToGP9:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GP9+1
    lda (SP)
    sta GP9
    rts

dpopToCFA:
    jsr incSP
    ldy #1
    lda (SP),y
    sta CFA+1
    lda (SP)
    sta CFA
    rts

dpopToGPTEMP:
    jsr incSP
    ldy #1
    lda (SP),y
    sta GPTEMP+1
    lda (SP)
    sta GPTEMP
    rts

;-----------------------------------------------------------------------------
; Forth return stack push/pop
;-----------------------------------------------------------------------------

incRP:
    clc
    lda RP
    adc #2
    sta RP
    bcc @skip
    inc RP+1
@skip:
    rts

decRP:
    sec
    lda RP
    sbc #2
    sta RP
    bcs @skip
    dec RP+1
@skip:
    rts

rpush:
    sta (RP)
    txa
    ldy #1
    sta (RP),y
    jmp decRP

rpop:
    jsr incRP
    ldy #1
    lda (RP),y
    tax
    lda (RP)
    rts

;-----------------------------------------------------------------------------
; IP Manipulation Routines
;-----------------------------------------------------------------------------

incIP:
    clc
    lda IP
    adc #2
    sta IP
    bcc @skip
    inc IP+1
@skip:
    rts

;-----------------------------------------------------------------------------
; GP Manipulation Routines
;-----------------------------------------------------------------------------

incGP0:
    .inc16 GP0
    rts

decGP0:
    .dec16 GP0
    rts

incGP1:
    .inc16 GP1
    rts

decGP1:
    .dec16 GP1
    rts

incGP2:
    .inc16 GP2
    rts

decGP2:
    .dec16 GP2
    rts

incGP3:
    .inc16 GP3
    rts

decGP3:
    .dec16 GP3
    rts

incGP4:
    .inc16 GP4
    rts

decGP4:
    .dec16 GP4
    rts

incGP5:
    .inc16 GP5
    rts

decGP5:
    .dec16 GP5
    rts

incGP6:
    .inc16 GP6
    rts

decGP6:
    .dec16 GP6
    rts

incGP7:
    .inc16 GP7
    rts

decGP7:
    .dec16 GP7
    rts

incGP8:
    .inc16 GP8
    rts

decGP8:
    .dec16 GP8
    rts

incGP9:
    .inc16 GP9
    rts

decGP9:
    .dec16 GP9
    rts

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

;*****************************************************************************
; Forth Runtime
;*****************************************************************************

DOCOLON:
    lda IP
    ldx IP+1
    jsr rpush
    clc
    lda CFA
    adc #2
    sta IP
    lda CFA+1
    bcc @skip
    inc a
@skip:
    sta IP+1
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
    phy
    jsr dpush
    ply
    dey
    lda (CFA),y
    tax
    dey
    lda (CFA),y
    jmp PUSHNEXT

DODOES:
    lda IP
    ldx IP+1
    jsr rpush
    pla
    sta IP
    pla
    sta IP+1
    .inc16 IP
    jmp DOVAR

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
    lda CFA+1
    sta GP0+1
    lda CFA
    sta GP0
    sec
    sbc #2
    sta GP0
    bcs @skip
    dec GP0+1
@skip:
    jsr dpop
    ldy #0
    sta (GP0),y
    iny
    txa
    sta (GP0),y
    jmp NEXT

DOVALUEPSTORE:
    lda CFA+1
    sta GP0+1
    lda CFA
    sta GP0
    sec
    sbc #4
    sta GP0
    bcs @skip
    dec GP0+1
@skip:
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
     ldy #2
     lda (CFA),y
     sta GP0
     iny
     lda (CFA),y
     sta GP0+1
     lda (GP0),y
     tax
     dey
     lda (GP0),y
     phy
     jsr dpush
     ply
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
    bcc @skip
    inc CFA+1
@skip:
    lda CFA
    sta CONTEXT+2
    lda CFA+1
    sta CONTEXT+3
    jmp NEXT


;*****************************************************************************
; Low-level File I/O
;*****************************************************************************

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
    .constword "3",THREE,3
    .constword "4",FOUR,4
    .constword "5",FIVE,5
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

    .constword "FORTH-KERNEL-START",FORTHKERNELSTART,ENTRY

    ; for the assembler
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
    stx GPTEMP
    ora GPTEMP
    bne @skip
    bra BRANCH+2
@skip:
    jsr incIP
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
    cmp GP0
    bne @skip
    cpx GP0+1
    bne @skip
    jsr incIP
    bra @doit
@skip:
    jsr dpush
    lda (IP)
    tax
    ldy #1
    lda (IP),y
    sta IP+1
    txa
    sta IP
@doit:
    jmp NEXT

    .codeword "(ENDOF)",PENDOFP
    lda (IP)
    tax
    ldy #1
    lda (IP),y
    sta IP+1
    txa
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
    bmi @skip
    lda (IP)
    tax
    ldy #1
    lda (IP),y
    sta IP+1
    txa
    sta IP
    jmp NEXT
@skip:
    jsr incIP
    clc
    lda RP
    adc #6
    sta RP
    bcc @done
    inc RP+1
@done:
    jmp NEXT

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
    bmi @skip
    lda (IP)
    tax
    ldy #1
    lda (IP),y
    sta IP+1
    txa
    sta IP
    jmp NEXT
@skip:
    jsr incIP
    clc
    lda RP
    adc #6
    sta RP
    bcc @done
    inc RP+1
@done:
    jmp NEXT

    .codeword "(DO)",PDOP
    jsr dpopToGP0
    jsr dpopToGP1
doPDOP:
    ldy #1
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
    cmp GP1
    bne @skip
    lda GP0+1
    cmp GP1+1
    beq @dontdoit
@skip:
    bra doPDOP
@dontdoit:
    lda (IP)
    tax
    ldy #1
    lda (IP),y
    sta IP+1
    stx IP
    jmp NEXT

    .varword "LP",LP
    .word 0

    .codeword "((LOCALALLOC))",PPLOCALALLOCPP
    jsr dpopToGP0
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
    tax
    lda RP
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

;*****************************************************************************
; Memory Manipulation Routines
;*****************************************************************************

;-----------------------------------------------------------------------------
; Memory Move Routine
; On entry:
;   GP0 = source addr
;   GP1 = dest addr
;   GP2 = length
;-----------------------------------------------------------------------------
memMove:
    lda GP1+1
    cmp GP0+1
    beq @hequal
    bcs moveUp
    bra moveDown
@hequal:
    lda GP1
    cmp GP0
    beq @done
    bcs moveUp
    bra moveDown
@done:
    rts

;-----------------------------------------------------------------------------
; Memory Move Higher to Lower
; On entry:
;   GP0 = source addr
;   GP1 = dest addr
;   GP2 = length
;-----------------------------------------------------------------------------
moveDown:
    ldy #0
    ldx GP2+1
    beq @lastpage
@dopage:
    lda (GP0),y
    sta (GP1),y
    iny
    bne @dopage
    inc GP0+1
    inc GP1+1
    dex
    bne @dopage
@lastpage:
    ldx GP2
    beq @done
@lastpageloop:
    lda (GP0),y
    sta (GP1),y
    iny
    dex
    bne @lastpageloop
@done:
    rts

;-----------------------------------------------------------------------------
; Memory Move Lower to Higher
; On entry:
;   GP0 = source addr
;   GP1 = dest addr
;   GP2 = length
;-----------------------------------------------------------------------------
moveUp:
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
    beq @nextpage
    dey
    beq @lastpagebyte
@nextbyte:
    lda (GP0),y
    sta (GP1),y
    dey
    bne @nextbyte
@lastpagebyte:
    lda (GP0),y
    sta (GP1),y
@nextpage:
    dey
    dec GP0+1
    dec GP1+1
    dex
    bne @nextbyte
    rts

    .codeword "MOVE",MOVE
    jsr dpopToGP2
    jsr dpopToGP1
    jsr dpopToGP0
    jsr memMove
    jmp NEXT

    .deferword "CMOVE",CMOVE,MOVE
    .deferword "CMOVE>",CMOVER,MOVE

    .codeword "!",STORE
    jsr dpopToGP0
    jsr incSP
    lda (SP)
    sta (GP0)
    ldy #1
    lda (SP),y
    sta (GP0),y
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
    jsr dpopToGP0
    jsr incSP
    lda (SP)
    sta (GP0)
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
    jsr dpopToGP0
    lda (GP0)
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
    jsr dpopToGP0
    lda #$ff
    sta (GP0)
    ldy #1
    sta (GP0),y
    jmp NEXT

    .codeword "OFF",OFF
    jsr dpopToGP0
    lda #0
    sta (GP0)
    ldy #1
    sta (GP0),y
    jmp NEXT

    .colonword "TOGGLE",TOGGLE
    .word DUP,CFETCH,ROT,DOXOR,SWAP,CSTORE
    .word UNNEST

    .codeword "INCR",INCR
    jsr dpopToGP0
    lda (GP0)
    inc a
    sta (GP0)
    bne @skip
    ldy #1
    lda (GP0),y
    inc a
    sta (GP0),y
@skip:
    jmp NEXT

    .codeword "DECR",DECR
    jsr dpopToGP0
    sec
    lda (GP0)
    sbc #1
    sta (GP0)
    bcs @skip
    ldy #1
    lda (GP0),y
    dec a
    sta (GP0),y
@skip:
    jmp NEXT

    .codeword "CINCR",CINCR
    jsr dpopToGP0
    lda (GP0)
    inc a
    sta (GP0)
    jmp NEXT

    .codeword "CDECR",CDECR
    jsr dpopToGP0
    lda (GP0)
    dec a
    sta (GP0)
    jmp NEXT

    .codeword "FILL",FILL
    jsr dpopToGP0
    jsr dpopToGP1
    jsr dpopToGP2
    lda GP0
    ldx GP0+1
    beq @lastpage
@nextpage:
    ldy #0
@storebyte:
    sta (GP2),y
    dey
    bne @storebyte
    inc GP2+1
    dex
    bne @nextpage
@lastpage:
    ldy GP1
@storebyte2:
    beq @done
    dey
    sta (GP2),y
    bra @storebyte2
@done:
    jmp NEXT


;*****************************************************************************
; Stack Manipulation Routines
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

    .codeword "R>",FROMR
    jsr rpop
    jmp PUSHNEXT

    .codeword "R>DROP",FROMRDROP
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

    .codeword "2R>",TWOFROMR
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
    phy
    jsr dpush
    ply
    dey
    lda (RP),y
    tax
    dey
    lda (RP),y
    jmp PUSHNEXT

doDup:
    ldy #2
    lda (SP),y
    sta (SP)
    iny
    lda (SP),y
    ldy #1
    sta (SP),y
    jsr decSP
    jmp NEXT

    .codeword "DUP",DUP
    bra doDup

    .codeword "?DUP",QDUP
    ldy #2
    lda (SP),y
    iny
    ora (SP),y
    beq @skip
    bra doDup
@skip:
    jmp NEXT

    .codeword "DROP",DROP
    jsr incSP
    jmp NEXT

    .codeword "SWAP",SWAP
    jsr dpopToGP0
    jsr dpopToGP1
    lda GP0
    ldx GP0+1
    jsr dpush
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    .codeword "OVER",OVER
    ldy #5
    lda (SP),y
    tax
    dey
    lda (SP),y
    jmp PUSHNEXT

    .codeword "ROT",ROT
    jsr dpopToGP0
    jsr dpopToGP1
    jsr dpopToGP2
    lda GP1
    ldx GP1+1
    jsr dpush
    lda GP0
    ldx GP0+1
    jsr dpush
    lda GP2
    ldx GP2+1
    jmp PUSHNEXT

    .codeword "-ROT",NROT
    jsr dpopToGP0
    jsr dpopToGP1
    jsr dpopToGP2
    lda GP0
    ldx GP0+1
    jsr dpush
    lda GP2
    ldx GP2+1
    jsr dpush
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    .codeword "NIP",NIP
    jsr dpopToGP0
    jsr incSP
    lda GP0
    ldx GP0+1
    jmp PUSHNEXT

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
    bcc @skip
    inc SP+1
@skip:
    jmp NEXT

    .colonword "2DUP",TWODUP
    .word OVER,OVER
    .word UNNEST

    .colonword "ROLL",ROLL
    .word DUPTOR,PICK,SPFETCH,CELLPLUS,DUP,CELLPLUS
    .word FROMR,CELLS,MOVE,DROP
    .word UNNEST

    .colonword "2OVER",TWOOVER
    .word LIT,3,PICK,LIT,3,PICK
    .word UNNEST

    .codeword "2SWAP",TWOSWAP
    jsr dpopToGP4
    jsr dpopToGP3
    jsr dpopToGP2
    jsr dpopToGP1
    lda GP3
    ldx GP3+1
    jsr dpush
    lda GP4
    ldx GP4+1
    jsr dpush
    lda GP1
    ldx GP1+1
    jsr dpush
    lda GP2
    ldx GP2+1
    jmp PUSHNEXT

    .colonword "DEPTH",DEPTH
    .word SPFETCH,SP0,FETCH,SWAP,MINUS,TWOSLASH
    .word UNNEST

;*****************************************************************************
; Math
;*****************************************************************************

    .codeword "1+",ONEPLUS
    ldy #2
    lda (SP),y
    ina
    sta (SP),y
    bne @skip
    iny
    lda (SP),y
    ina
    sta (SP),y
@skip:
    jmp NEXT

    .deferword "CHAR+",CHARPLUS,ONEPLUS

    .codeword "1-",ONEMINUS
    sec
    ldy #2
    lda (SP),y
    sbc #1
    sta (SP),y
    bcs @skip
    iny
    lda (SP),y
    sbc #0
    sta (SP),y
@skip:
    jmp NEXT

    .deferword "CHAR-",CHARMINUS,ONEMINUS

    .codeword "CHARS",CHARS
    jmp NEXT

    .codeword "2+",TWOPLUS
    clc
    ldy #2
    lda (SP),y
    adc #2
    sta (SP),y
    bcc @skip
    iny
    lda (SP),y
    ina
    sta (SP),y
@skip:
    jmp NEXT

    .codeword "2-",TWOMINUS
    sec
    ldy #2
    lda (SP),y
    sbc #2
    sta (SP),y
    bcs @skip
    iny
    lda (SP),y
    sbc #0
    sta (SP),y
@skip:
    jmp NEXT

    .codeword "CELL+",CELLPLUS
    bra TWOPLUS+2

    .codeword "CELL-",CELLMINUS
    bra TWOMINUS+2

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
mul32x32:
    mul32x32mulnd   = GP2
    mul32x32mulr    = GP0
    mul32x32prod    = GP4

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
@shift:
    lsr mul32x32mulr+3
    ror mul32x32mulr+2
    ror mul32x32mulr+1
    ror mul32x32mulr+0
    bcc @rotate
    clc
    lda mul32x32prod+4
    adc mul32x32mulnd+0
    sta mul32x32prod+4
    lda mul32x32prod+5
    adc mul32x32mulnd+1
    sta mul32x32prod+5
    lda mul32x32prod+6
    adc mul32x32mulnd+2
    sta mul32x32prod+6
    lda mul32x32prod+7
    adc mul32x32mulnd+3
@rotate:
    ror a
    sta mul32x32prod+7
    ror mul32x32prod+6
    ror mul32x32prod+5
    ror mul32x32prod+4
    ror mul32x32prod+3
    ror mul32x32prod+2
    ror mul32x32prod+1
    ror mul32x32prod+0
    dex
    bne @shift
    clc
    rts

;-----------------------------------------------------------------------------
; Multiply 16-bit unsigned number by a 16-bit unsigned number, 32-bit result
; On Entry:
;	u1 - GP2 (lo)
;	u2 - GP0 (lo)
; On Exit:
;	ud - GP4 (lo), GP5 (hi)
;-----------------------------------------------------------------------------
mul16x16:
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
@shift:
    ror mul16x16mulr+1
    ror mul16x16mulr+0
    bcc @rotate
    clc
    lda mul16x16prod+2
    adc mul16x16mulnd+0
    sta mul16x16prod+2
    lda mul16x16prod+3
    adc mul16x16mulnd+1
@rotate:
    ror a
    sta mul16x16prod+3
    ror mul16x16prod+2
    ror mul16x16prod+1
    ror mul16x16prod+0
    dex
    bne @shift
    clc
    rts

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
div32x32:
    div32x32dividend = GP0
    div32x32divisor  = GP2
    div32x32rem      = GP4
    div32x32temp     = GP6

    stz div32x32rem+0
    stz div32x32rem+1
    stz div32x32rem+2
    stz div32x32rem+3
    ldx #32
@loop:
    asl div32x32dividend+0
    rol div32x32dividend+1
    rol div32x32dividend+2
    rol div32x32dividend+3
    rol div32x32rem+0
    rol div32x32rem+1
    rol div32x32rem+2
    rol div32x32rem+3
    lda div32x32rem+0
    sec
    sbc div32x32divisor+0
    tay
    lda div32x32rem+1
    sbc div32x32divisor+1
    sta div32x32temp+0
    lda div32x32rem+2
    sbc div32x32divisor+2
    sta div32x32temp+1
    lda div32x32rem+3
    sbc div32x32divisor+3
    bcc @skip
    sta div32x32rem+3
    lda div32x32temp+1
    sta div32x32rem+2
    lda div32x32temp+0
    sta div32x32rem+1
    sty div32x32rem+0
    inc div32x32dividend
@skip:
    dex
    bne @loop
    rts

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
div16x16:
    div16x16dividend = GP0
    div16x16divisor  = GP2
    div16x16rem      = GP4
    div16x16temp     = GP6

    stz div16x16rem+0
    stz div16x16rem+1
    ldx #16
@loop:
    asl div16x16dividend+0
    rol div16x16dividend+1
    rol div16x16rem+0
    rol div16x16rem+1
    lda div16x16rem+0
    sec
    sbc div16x16divisor+0
    tay
    lda div16x16rem+1
    sbc div16x16divisor+1
    bcc @skip
    sta div16x16rem+1
    sty div16x16rem+0
    inc div16x16dividend+0
@skip:
    dex
    bne @loop
    rts

;-----------------------------------------------------------------------------

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
    jsr dpopToGP0
    jsr dpopToGP1
    sec
    lda GP1
    sbc GP0
    sta GP0
    lda GP1+1
    sbc GP0+1
    tax
    lda GP0
    jmp PUSHNEXT

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
@loop:
    beq @done
    asl GP1
    rol GP1+1
    dex
    bra @loop
@done:
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    .codeword "RSHIFT",RSHIFT
    jsr dpopToGP0
    jsr dpopToGP1
    clc
    ldx GP0
@loop:
    beq @done
    lsr GP1+1
    ror GP1
    dex
    bra @loop
@done:
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    .codeword "=",EQUAL
    jsr incSP
    ldx #0
    ldy #1
    lda (SP),y
    ldy #3
    eor (SP),y
    bne @skip
    lda (SP)
    dey
    eor (SP),y
    bne @skip
    dex
@skip:
    txa
    ldy #2
    sta (SP),y
    iny
    sta (SP),y
    jmp NEXT

    .codeword "0=",ZEQUAL
    ldx #0
    ldy #2
    lda (SP),y
    iny
    ora (SP),y
    bne @skip
    dex
@skip:
    txa
    sta (SP),y
    dey
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
    beq @skip
    dex
@skip:
    txa
    ldy #3
    sta (SP),y
    dey
    sta (SP),y
    jmp NEXT

    .codeword "0>",ZGREATER
    ldx #0
    ldy #3
    lda (SP),y
    bmi @done
    bne @skip
    dey
    lda (SP),y
    beq @done
@skip:
    dex
@done:
    txa
    ldy #2
    sta (SP),y
    iny
    sta (SP),y
    jmp NEXT

    .colonword "<",LESS
    .word MINUS,ZLESS
    .word UNNEST

    .colonword ">",GREATER
    .word SWAP,MINUS,ZLESS
    .word UNNEST

    .codeword "U<",ULESS
    jsr dpopToGP2
    jsr dpopToGP1
    ldx #$ff
    lda GP1+1
    cmp GP2+1
    bcc @less
    bne @notless
    lda GP1
    cmp GP2
    bcc @less
@notless:
    inx
@less:
    txa
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
    lda #$ff
    tax
    jsr dpush
    jmp DOXOR+2

    .colonword "+-",PLUSMINUS
    .word ZLESS,QBRANCH,@skip,NEGATE,PTHENP
@skip:
    .word UNNEST

    .colonword "D+=",DPLUSMINUS
    .word ZLESS,QBRANCH,@skip,DNEGATE,PTHENP
@skip:
    .word UNNEST

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
    lda GP4
    ldx GP4+1
    jsr dpush
    lda GP5
    ldx GP5+1
    jsr dpush
    lda GP0
    ldx GP0+1
    jsr dpush
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    .colonword "*",STAR
    .word TWODUP,DOXOR,TOR,DOABS,SWAP,DOABS,USTAR,FROMR,PLUSMINUS
    .word UNNEST

    .colonword "M*",MSTAR
    .word TWODUP,DOXOR,TOR,DOABS,SWAP,DOABS,UMSTAR,FROMR,DPLUSMINUS
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
    .word UMSLASHMOD,SWAP,DROP
    .word UNNEST

    .colonword "UMMOD",UMMOD
    .word UMSLASHMOD,DROP
    .word UNNEST

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
    .word TWODUP,DOXOR,TOR,DOABS,USLASH,FROMR,PLUSMINUS
    .word UNNEST

    .colonword "MOD",MOD
    .word TWODUP,DOXOR,TOR,DOABS,UMOD,FROMR,PLUSMINUS
    .word UNNEST

    .colonword "/MOD",SLASHMOD
    .word TWODUP,DOXOR,TOR,DOABS,USLASHMOD,FROMR,PLUSMINUS
    .word UNNEST

    .colonword "S>D",STOD
    .word DUP,ZLESS
    .word UNNEST

    .colonword "U>D",UTOD
    .word ZERO
    .word UNNEST

    .colonword "*/MOD",STARSLASHMOD
    .word TOR,MSTAR,DUP,ZLESS,TOR
    .word DODABS,FROMR,FROMR,SWAP,TOR,DUP,ZLESS,TOR,DOABS
    .word UMSLASHMOD,FROMR,FROMR,DOXOR,PLUSMINUS
    .word UNNEST

    .colonword "*/",STARSLASH
    .word STARSLASHMOD,SWAP,DROP
    .word UNNEST

    .colonword "SM/REM",SMSLASHREM
    .word DUP,ZLESS,TOR,TOR,DUP,ZLESS,FROMR,SWAP,TOR,TOR
    .word DODABS,FROMR,DOABS,UMSLASHMOD
    .word FROMR,FROMR,DOXOR,PLUSMINUS
    .word UNNEST

    .colonword "MIN",MIN
    .word TWODUP,GREATER
    .word QBRANCH,@skip,SWAP,PTHENP
@skip:
    .word DROP
    .word UNNEST

    .colonword "MAX",MAX
    .word TWODUP,LESS
    .word QBRANCH,@skip,SWAP,PTHENP
@skip:
    .word DROP
    .word UNNEST

    .colonword "0MAX",ZMAX
    .word ZERO,MAX
    .word UNNEST

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
    .word LIT,2,PICK,MIN,TOR,MAX,FROMR,EQUAL
    .word UNNEST

    .colonword "WITHIN",WITHIN
    .word LIT,2,PICK,SWAP,ULESS,NROT,TWODUP,EQUAL,NROT
    .word GREATER,DOOR,DOAND
    .word UNNEST


;*****************************************************************************
; Core Words
;*****************************************************************************

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
    bcc @skip
    inc RP+1
@skip:
    lda (RP)
    sta IP
    ldy #1
    lda (RP),y
    sta IP+1
    jmp NEXT

    .codeword "?LEAVE",QLEAVE
    jsr dpop
    stx GPTEMP
    ora GPTEMP
    beq @skip
    jmp LEAVE+2
@skip:
    jmp NEXT

getLoopIndex:
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

    .codeword "I",I
    ldy #2
    bra getLoopIndex

    .codeword "J",J
    ldy #8
    bra getLoopIndex

    .codeword "UNLOOP",UNLOOP
    clc
    lda RP
    adc #6
    sta RP
    bcc @skip
    inc RP+1
@skip:
    jmp NEXT

    .codeword "NOOP",NOOP
    jmp NEXT

    .codeword "BYE",BYE
    brk #$55
    jmp NEXT

    .codeword "HALT",HALT
    stp

    .codeword "((\"))",PPQUOTEPP
    jsr rpop
    sta GP0
    stx GP0+1
    jsr dpush
    lda (GP0)
    clc
    adc GP0
    sta GP0
    bcc @skip
    inc GP0+1
@skip:
    inc GP0
    bne @skip2
    inc GP0+1
@skip2:
    lda GP0
    ldx GP0+1
    jsr rpush
    jmp NEXT

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
    bne @skip
    inc GP0+1
@skip:
    lda GP0+1
    sta (SP),y
    dey
    lda GP0
    sta (SP),y
    txa
    ldx #0
    jmp PUSHNEXT

;*****************************************************************************
; Forth Terminal Support
;*****************************************************************************

    .codeword "(EMIT)",PEMITP
    jsr dpop
    jsr CHROUT
    jmp NEXT

    .deferword "EMIT",EMIT,PEMITP

    .codeword "(TYPE)",PTYPEP
    jsr dpopToGP0
    jsr dpopToGP1
@loop:
    lda GP0
    ora GP0+1
    beq @done
    lda (GP1)
    jsr CHROUT
    jsr incGP1
    jsr decGP0
    bra @loop
@done:
    jmp NEXT

    .deferword "TYPE",TYPE,PTYPEP

    .codeword "(CLS)",PCLSP
    jsr CLRSCR
    jmp NEXT

    .deferword "CLS",CLS,PCLSP

    .codeword "(GOTOXY)",PGOTOXYP
    jsr dpop
    pha
    jsr dpop
    tax
    ply
    jsr SETCURXY
    jmp NEXT

    .deferword "GOTOXY",GOTOXY,PGOTOXYP

    .codeword "(GETXY)",PGETXYP
    jsr GETCURX
    ldx #0
    jsr dpush
    jsr GETCURY
    ldx #0
    jmp PUSHNEXT

    .deferword "GETXY",GETXY,PGETXYP

    .codeword "(GETCOLROW)",PGETCOLROWP
    jsr GETSCRW
    ldx #0
    jsr dpush
    jsr GETSCRH
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
doAccept:
    stz GP0
    stz GP0+1
@nextch:
    jsr CHRIN
    cmp #08
    beq @bksp
    cmp #$7f
    bne @notbksp
@bksp:
    lda GP0
    ora GP0+1
    beq @nextch
    jsr decGP0
    jsr decGP1
    lda #08
    jsr CHROUT
    bra @nextch
@notbksp:
    cmp #$0d
    beq @done
    cmp #$0a
    beq @done
    tax
    lda GP2
    cmp GP0
    bne @storechar
    lda GP2+1
    cmp GP0+1
    bne @storechar
    lda #07
    jsr CHROUT
    bra @nextch
@storechar:
    txa
    sta (GP1)
    jsr CHROUT
    jsr incGP1
    jsr incGP0
    bra @nextch
@done:
    rts

    .codeword "(ACCEPT)",PACCEPTP
    jsr dpopToGP2
    jsr dpopToGP1
    jsr doAccept
    lda GP0
    ldx GP0+1
    jmp PUSHNEXT

    ; ( addr len -- len )
    .deferword "ACCEPT",ACCEPT,PACCEPTP

    .codeword "(KEY?)",PKEYQP
    jsr CHRCHECK
    lda #$ff
    bcs @done
    inc a
@done:
    tax
    jmp PUSHNEXT

    ; ( -- f )
    .deferword "KEY?",KEYQ,PKEYQP

    .codeword "(KEY)",PKEYP
    jsr CHRIN
    ldx #0
    jmp PUSHNEXT

    ; ( -- c )
    .deferword "KEY",KEY,PKEYP

    .colonword "SPACE",SPACE
    .word BL,EMIT
    .word UNNEST

    .constword "SPCS-MAX",SPCSMAX,128
    .varword "SPCS",SPCS
    .res 128,32

    .colonword "SPACES",SPACES
    .word PBEGINP
@loop:
        .word DUP,ZGREATER
    .word PWHILEP,@done
        .word DUP,SPCSMAX,MIN,SPCS,OVER,TYPE,MINUS
    .word PREPEATP,@loop
@done:
    .word DROP
    .word UNNEST

    .codeword "(CR)",PCRP
    lda #$0d
    jsr CHROUT
    jmp NEXT

    .deferword "CR",CR,PCRP

;*****************************************************************************
; System Stuff
;*****************************************************************************
    ; TODO: This needs to be replaced when a real timer is implemented
    .codeword "(MS)",PMSP
    jsr dpop
    sta GP0
    stx GP0+1
    ora GP0+1
    beq @done
@loop:
    lda #0
    ldy #150
@loop2:
    cpy #1
    dey
    sbc #0
    bcs @loop2
    jsr decGP0
    lda GP0
    ora GP0+1
    bne @loop
@done:
    jmp NEXT

    ; ( ms -- )
    .deferword "MS",MS,PMSP

;*****************************************************************************
; Forth String Words
;*****************************************************************************

    .codeword "CLIP$",CLIPSTRING
    ldy #3
    lda (SP),y
    beq @done
    bpl @notneg
    lda #0
    sta (SP),y
    dey
    sta (SP),y
    bra @done
@notneg:
    lda #0
    sta (SP),y
    dey
    dec a
    sta (SP),y
@done:
    jmp NEXT

    .colonword "PLACE",PLACE
    .word SWAP,CLIPSTRING,SWAP,TWODUP,TWOTOR,CHARPLUS,SWAP
    .word MOVE,TWOFROMR,CSTORE
    .word UNNEST

    .colonword "+PLACE",PLUSPLACE
    .word TOR,CLIPSTRING,MAXCOUNTED,RFETCH,CFETCH,MINUS,MIN
    .word FROMR,TWODUP,TWOTOR,COUNT,CHARS,PLUS,SWAP,MOVE
    .word TWOFROMR,CPLUSSTORE
    .word UNNEST

    .codeword "SKIP",SKIP
    jsr dpopToGP0
    jsr dpopToGP1
    jsr dpopToGP2
@loop:
    lda GP1
    ora GP1+1
    beq @done
    lda (GP2)
    cmp GP0
    bne @done
    jsr incGP2
    jsr decGP1
    bra @loop
@done:
    lda GP2
    ldx GP2+1
    jsr dpush
    lda GP1
    ldx GP1+1
    jmp PUSHNEXT

    ; ( addr len char -- addr' len' )
    ; scan string for first occurance of char
    .codeword "SCAN",SCAN
    jsr dpopToGP0
    jsr dpopToGP1
    jsr dpopToGP2
@loop:
    lda GP1
    ora GP1+1
    beq @done
    lda (GP2)
    cmp GP0
    beq @done
    jsr incGP2
    jsr decGP1
    bra @loop
@done:
    lda GP2
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
doUppercase:
@loop:
    lda GP2
    ora GP2+1
    beq @done
    lda (GP1)
    cmp #'a'-1
    bcc @next
    cmp #'z'+1
    bcs @next
    and #$df
    sta (GP1)
@next:
    jsr incGP1
    jsr decGP2
    bra @loop
@done:
    rts

    ; ( addr u -- )
    .codeword "UPPER",UPPER
    jsr dpopToGP2
    jsr dpopToGP1
    jsr doUppercase
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
    jsr doUppercase
    jmp NEXT

    ; ( addr u n -- addr' u' )
    .colonword "/STRING",SLASHSTRING
    .word OVER,MIN,DUPTOR,MINUS,SWAP,FROMR,CHARS,PLUS,SWAP
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
doCompare:
    lda GP2
    eor GP4
    bne @notequal
    lda GP2+1
    eor GP4+1
    bne @notequal
    bra @begin
@loop:
    lda (GP1)
    cmp (GP3)
    beq @continue
    bra @notequal
@continue:
    jsr decGP2
    jsr decGP4
    jsr incGP1
    jsr incGP3
@begin:
    lda GP2
    ora GP2+1
    sta GP0
    lda GP4
    ora GP4+1
    sta GP0+1
    ldx #0
    lda GP0
    ora GP0+1
    beq @done
    lda GP0
    beq @short1
    lda GP0+1
    beq @short2
    bra @loop
@notequal:
    bcs @short2
@short1:
    lda #$ff
    tax
    bra @done
@short2:
    lda #$01
    ldx #$00
@done:
    rts

    .codeword "COMPARE",COMPARE
    jsr dpopToGP4
    jsr dpopToGP3
    jsr dpopToGP2
    jsr dpopToGP1
    jsr doCompare
    jmp PUSHNEXT

;*****************************************************************************
; Forth Exception Words
;*****************************************************************************

    .varword "HANDLER",HANDLER
    .word 0

    ; ( xt -- f )
    .colonword "CATCH",CATCH
    .word SPFETCH,TOR,LP,FETCH,TOR,HANDLER,FETCH,TOR,RPFETCH,HANDLER,STORE
    .word EXECUTE
    .word FROMR,HANDLER,STORE,FROMR,DROP,FROMR,DROP,ZERO
    .word UNNEST

    ; ( n -- )
    .colonword "THROW",THROW
    .word QDUP
    .word QBRANCH,@done
        .word HANDLER,FETCH,RPSTORE,FROMR,HANDLER,STORE,FROMR,LP,STORE
        .word FROMR,SWAP,TOR,SPSTORE,DROP,FROMR
    .word PTHENP
@done:
    .word UNNEST

    .colonword "?THROW",QTHROW
    .word SWAP
    .word QBRANCH,@else
        .word THROW
    .word BRANCH,@then
@else:
        .word DROP
    .word PTHENP
@then:
    .word UNNEST

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
    .word THROW_STACKUNDER
    .ptext "stack underflow"
TMSG02:
    .word TMSG01
    .word THROW_UNDEFINED
    .ptext "is undefined"
TMSG03:
    .word TMSG02
    .word THROW_COMPONLY
    .ptext "is compilation only"
TMSG04:
    .word TMSG03
    .word THROW_NAMEREQD
    .ptext "requires a name"
TMSG05:
    .word TMSG04
    .word THROW_MISMATCH
    .ptext "control structure mismatch"
TMSG06:
    .word TMSG05
    .word THROW_FILENOTFOUND
    .ptext "file not found"
TMSG40:
    .word TMSG06
    .word THROW_NOTDEFER
    .ptext "not defered"
TMSG41:
    .word TMSG40
    .word THROW_NOTVALUE
    .ptext "not value"
TMSG42:
    .word TMSG41
    .word THROW_OUTOFMEM
    .ptext "out of memory"
TMSG43:
    .word TMSG42
    .word THROW_FILECREATEFAIL
    .ptext "file create failed"
TMSG44:
    .word TMSG43
    .word THROW_FILEREADFAIL
    .ptext "file read failed"
TMSG45:
    .word TMSG44
    .word THROW_FILEWRITEFAIL
    .ptext "file write failed"
TMSG46:
    .word TMSG45
    .word THROW_EXECONLY
    .ptext "is execution only"
TMSG47:
    .word TMSG46
    .word THROW_LOCALSTWICE
    .ptext "locals defined twice"
TMSG48:
    .word TMSG47
    .word THROW_TOOMANYLOCALS
    .ptext "too many locals"
TMSG49:
    .word TMSG48
    .word THROW_LOCALSNOCLOSE
    .ptext "locals missing }"
TMSG50:
    .word TMSG49
    .word THROW_STACKCHG
    .ptext "stack changed"
TMSG51:
    .word TMSG50
    .word THROW_ARGSNODASHDASH
    .ptext "locals missing --"
TMSG52:
    .word TMSG51
    .word THROW_NOTLOCAL
    .ptext "is not a LOCAL"
TMSG53:
    .word TMSG52
    .word THROW_FILECLOSEFAIL
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
; Number Input
;*****************************************************************************

    .varword "BASE",BASE
    .word 10

    .colonword "DECIMAL",DECIMAL
    .word LIT,10,BASE,STORE
    .word UNNEST

    .colonword "HEX",HEX
    .word LIT,16,BASE,STORE
    .word UNNEST

    .colonword "OCTAL",OCTAL
    .word LIT,8,BASE,STORE
    .word UNNEST

    .colonword "BINARY",BINARY
    .word LIT,2,BASE,STORE
    .word UNNEST

    .valueword "?DOUBLE",QDOUBLE,0
    .valueword "DP-LOCATION",DPLOCATION,$FFFF

    ; (char base -- n f)
    .codeword "DIGIT",DIGIT
    jsr dpopToGP0
    jsr dpopToGP1
    lda GP1
    sec
    sbc #48
    bcc @isnt
    cmp #9
    bcc @mightbe
    beq @mightbe
    sbc #7
    cmp #10
    bcc @done
@mightbe:
    cmp GP0
    bcs @isnt
    ldx #0
    jsr dpush
    lda #$ff
    tax
    bra @done
@isnt:
    lda GP1
    ldx #0
    jsr dpush
    lda #0
    tax
@done:
    jmp PUSHNEXT

    ; ( d1 addr len -- d2 addr2 len2 )
    .colonword ">NUMBER",TONUMBER
    .word TWODUP,BOUNDS
    .word PQDOP,@done
@loop:
        .word OVER,CFETCH,BASE,FETCH,DIGIT
        .word QBRANCH,@else
            .word NROT,TWOTOR,TOR,BASE,FETCH,STOD,UDSTAR
            .word FROMR,STOD,DPLUS
            .word TWOFROMR,SWAP,ONEPLUS,SWAP,ONEMINUS
        .word BRANCH,@then
@else:
            .word DROP,LEAVE
        .word PTHENP
@then:
    .word PLOOPP,@loop
@done:
    .word UNNEST

    ; ( addr len -- d f )
    .colonword "NUMNBER?",NUMBERQ
    .word FALSECON,QDOUBLE+4,LIT,$FFFF,DPLOCATION+4
    .word OVER,CFETCH,LIT,'-',EQUAL,OVER,ZGREATER,DOAND,DUPTOR
    .word QBRANCH,@L1
        .word ONE,SLASHSTRING
    .word PTHENP
@L1:
    .word DUP,ZEQUAL
    .word QBRANCH,@L2
        .word FROMRDROP,TWODROP,ZERO,ZERO,FALSECON,PEXITP
    .word PTHENP
@L2:
    .word ZERO,ZERO,TWOSWAP,TONUMBER,OVER,CFETCH,LIT,'.',EQUAL
    .word OVER,ZGREATER,DOAND
    .word QBRANCH,@L3
        .word DUP,ONEMINUS,DPLOCATION+4,ONE,SLASHSTRING
        .word TONUMBER,DUP,ZEQUAL
        .word QBRANCH,@L4
            .word TRUECON,QDOUBLE+4
        .word PTHENP
@L4:
    .word PTHENP
@L3:
    .word NIP,ZEQUAL,FROMR
    .word QBRANCH,@L5
        .word TOR,DNEGATE,FROMR
    .word PTHENP
@L5:
    .word UNNEST

    ; ( f -- )
    .colonword "?MISSING",QMISSING
    .word THROWUNDEFINED,QTHROW
    .word UNNEST

    .varword "CAPS",CAPS
    .word $FFFF

    .colonword "?UPPERCASE",QUPPERCASE
    .word CAPS,FETCH
    .word QBRANCH,@then
        .word UPPERCASE
    .word PTHENP
@then:
    .word UNNEST

    ; ( str -- d )
    .colonword "(NUMBER)",PNUMBERP
    .word COUNT,FINDBUFFER,PLACE
    .word FINDBUFFER,QUPPERCASE,COUNT,NUMBERQ,ZEQUAL,QMISSING
    .word UNNEST

    .deferword "NUMBER",NUMBER,PNUMBERP


;*****************************************************************************
; Forth Parser
;*****************************************************************************

	.varword "TIB",TIB
    .res 256,0

	.varword "(SOURCE)",PSOURCEP
	.word 0
	.word TIB+2

	.colonword "SOURCE",SOURCE
	.word PSOURCEP,TWOFETCH
	.word UNNEST

	.constword "#TIB",HASHTIB,PSOURCEP+2

	.varword ">IN",TOIN
	.word 0

    .varword "POCKET",POCKET
    .res 256,0

    .colonword "WORD",WORD
    .word TOR,TOIN,FETCH,DUP,PSOURCEP,TWOPLUS,FETCH,PLUS
    .word PSOURCEP,FETCH,ROT,MINUS,DUP,ZGREATER
    .word QBRANCH,@else
        .word RFETCH,SKIP,TWODUP,FROMR,SCAN
        .word PSOURCEP,FETCH,OVER,MINUS,ONEPLUS,TOIN,STORE
        .word SWAP,DROP,MINUS
    .word BRANCH,@then
@else:
        .word TWODROP,FROMR,DROP,ZERO,ZERO
    .word PTHENP
@then:
    .word POCKET,PLACE,POCKET
    .word UNNEST

    .colonword "PARSE",PARSE
    .word TOR,SOURCE,TOIN,FETCH,SLASHSTRING,TWODUP,FROMR,SCAN
    .word NIP,MINUS,DUP,ONEPLUS,TOIN,PLUSSTORE
    .word UNNEST

;*****************************************************************************
; QUERY portion of QUERY-INTERPRET
;*****************************************************************************

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
; Runtime Primitives Continued
;*****************************************************************************

    .colonword "(.\")",PDOTQUOTEP
    .word PPQUOTEPP,COUNT,TYPE
    .word UNNEST

    .colonword "(S\")",PSQUOTEP
    .word PPQUOTEPP,COUNT
    .word UNNEST

    .colonword "(C\")",PCQUOTEP
    .word PPQUOTEPP
    .word UNNEST

    .colonword "_(",UNDERPAREN
    .word LIT,')',PARSE,TWODROP
    .word UNNEST

    .icolonword ".(",DOTPAREN
    .word LIT,')',PARSE,TYPE
    .word UNNEST

    .varword "HLD",HLD
    .res 82,0

    .valueword "PAD",PAD,HLD+80+2

    .codeword "B>C",BTOC
    ldy #2
    lda (SP),y
    cmp #10
    bcc @skip
    clc
    adc #7
@skip:
    clc
    adc #'0'
    sta (SP),y
    lda #0
    iny
    sta (SP),y
    jmp NEXT

    .colonword "HOLD",HOLD
    .word HLD,FETCH,ONEMINUS,DUP,HLD,STORE,CSTORE
    .word UNNEST

    .colonword "SIGN",SIGN
    .word ZLESS
    .word QBRANCH,@then
        .word LIT,'-',HOLD
    .word PTHENP
@then:
    .word UNNEST

    .colonword "<#",BEGNUMBER
    .word PAD,HLD,STORE
    .word UNNEST

    .colonword "#",NUMBERSIGN
    .word BASE,FETCH,ZERO,UDSLASHMOD,TWOSWAP,DROP,BTOC,HOLD
    .word UNNEST

    .colonword "#S",NUMBERSIGNS
    .word PBEGINP
@loop:
        .word NUMBERSIGN,TWODUP,DOOR,ZEQUAL
    .word QBRANCH,@loop
    .word UNNEST

    .colonword "#>",NUMBEREND
    .word TWODROP,HLD,FETCH,PAD,OVER,MINUS
    .word UNNEST

    .colonword "(D.)",PDDOTP
    .word TUCK,DODABS,BEGNUMBER,NUMBERSIGNS,ROT,SIGN,NUMBEREND
    .word UNNEST

    .colonword "D.",DDOT
    .word PDDOTP,TYPE,SPACE
    .word UNNEST

    .colonword ".",DOT
    .word STOD,DDOT
    .word UNNEST

    .colonword "U.",UDOT
    .word ZERO,DDOT
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
    .res NUMVOCSDEF*2,0

    .varword "CURRENT",CURRENT
    .word FORTH+4

    .varword "FIND-BUFFER",FINDBUFFER
    .res 256,0

;-----------------------------------------------------------------------------
; Traverse from nfa to lfa
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
;-----------------------------------------------------------------------------
doNtoLink:
    sta GP0
    stx GP0+1
    lda (GP0)
    and #63
    clc
    adc GP0
    sta GP0
    bcc @skip
    inc GP0+1
    clc
@skip:
    lda #3
    adc GP0
    sta GP0
    bcc @skip2
    inc GP0+1
@skip2:
    lda GP0
    ldx GP0+1
    rts

;-----------------------------------------------------------------------------
; Search wordlist wlst for the word at addr,len
;
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
;-----------------------------------------------------------------------------
doSearch1WordList:
@s1wbegin:
    lda GP3
    ora GP3+1               ; end of wordlist?
    bne @s1w0               ; no, continue
    bra @s1wno              ; yes, not match for you
@s1w0:
    lda (GP3)
    and #63                 ; length of current word
    cmp GP2                 ; is it same as search word length?
    bne @s1wnext            ; no, try next word
    clc
    lda GP3
    adc #1
    sta GP5
    lda GP3+1
    adc #0
    sta GP5+1
    ldy GP2
@s1w1:
    dey
    bmi @s1wmatch           ; end of string, so it matches
    lda (GP1),y
    cmp (GP5),y             ; compare search word to current word
    bne @s1wnext            ; not a match, try another
    bra @s1w1
@s1wnext:
    lda GP3                 ; move to next word in wordlist
    ldx GP3+1
    jsr doNtoLink           ; name -> lfa
    sta GP3
    stx GP3+1
    ldy #1                  ; lfa -> next word
    lda (GP3),y
    tax
    lda (GP3)
    sta GP3
    txa
    sta GP3+1
    bra @s1wbegin
@s1wmatch:
    lda (GP3)               ; get length|immed byte
    sta GP4
    and #63
    clc
    adc GP3                 ; wlst + len
    sta GP3
    bcc @s1wmatcha
    inc GP3+1
    clc
@s1wmatcha:
    lda GP3
    adc #5                  ; wlst -> cfa (1 for count, 2 for nla, 2 for lfa)
    sta GP3
    bcc @s1wmatchb
    inc GP3+1
@s1wmatchb:
    sec                     ; it's a match
    lda GP4
    and #$80                ; is it immediate?
    bne @s1wimmed
    dec a                   ; it's a regular word (0 -> -1)
    tax
    bra @s1wdone
@s1wimmed:
    lda #1                  ; it's and immediate word
    ldx #0
    bra @s1wdone
@s1wno:
    clc                     ; no match for you
@s1wdone:
    sta GP2
    stx GP2+1
    rts

    ; ( addr len wlst -- 0 | cfa 1 | cfa -1
    ; uses GP1,GP2,GP3
    .codeword "SEARCH-ONE-WORDLIST",SEARCH1WORDLIST
    jsr dpopToGP3
    jsr dpopToGP2
    jsr dpopToGP1
    jsr doSearch1WordList
    bcc @notFound
    lda GP3
    ldx GP3+1
    jsr dpush
    lda GP2
    ldx GP2+1
    bra @done
@notFound:
    lda #0
    tax
@done:
    jmp PUSHNEXT

    .colonword "(SEARCH-WORDLIST)",PSEARCHWORDLISTP
    .word FETCH,SEARCH1WORDLIST
    .word UNNEST

    .colonword "(FIND)",PFINDP
    .word DUP,CFETCH,ZEQUAL
    .word QBRANCH,@then1
        .word ZERO,PEXITP
    .word PTHENP
@then1:
    .word CONTEXT
    .word PBEGINP
@loop:
        .word DUP,FETCH
    .word PWHILEP,@endloop
        .word DUP,TWOFETCH,NOTEQUAL
        .word QBRANCH,@then3
            .word OVER,COUNT,MAXNAMECHARS,MIN,LIT,2,PICK,FETCH
            .word PSEARCHWORDLISTP,QDUP
            .word QBRANCH,@then2
                .word TWOSWAP,TWODROP,PEXITP
            .word PTHENP
@then2:
        .word PTHENP
@then3:
        .word CELLPLUS
    .word PREPEATP,@loop
@endloop:
    .word DROP,FALSECON
    .word UNNEST

    ; ( cstr -- cstr 0 | cfa 1 | cfa -1 )
    .colonword "CAPS-FIND",CAPSFIND
    .word DUP,COUNT,FINDBUFFER,PLACE,FINDBUFFER,QUPPERCASE,PFINDP
    .word DUPTOR
    .word QBRANCH,@else
        .word NIP
    .word BRANCH,@then
@else:
        .word DROP
    .word PTHENP
@then:
    .word FROMR
    .word UNNEST

    .deferword "FIND",FIND,CAPSFIND

    .colonword "DEFINED",DEFINED
    .word BL,WORD,FIND
    .word UNNEST

    .colonword "'",TICK
    .word DEFINED,ZEQUAL,QMISSING
    .word UNNEST

;*****************************************************************************
; Forth Dictionary Support Words
;*****************************************************************************

    ; ( nfa -- lfa )
    .codeword "N>LINK",NTOLINK
    jsr dpop
    jsr doNtoLink
    jmp PUSHNEXT

    ; ( nfa -- addr len )
    .colonword "NFA-COUNT",NFACOUNT
    .word DUP,ONEPLUS,SWAP,CFETCH,MAXNAMECHARS,DOAND
    .word UNNEST

    ; ( nfa -- cfa )
    .colonword "NAME>",NAMETO
    .word NFACOUNT,PLUS,TWO,CELLSPLUS
    .word UNNEST

    ; ( cfa -- nfa )
    .colonword ">NAME",TONAME
    .word TWO,CELLSMINUS,FETCH
    .word UNNEST

    ; ( pfa -- cfa )
    .colonword "BODY>",BODYTO
    .word CELLMINUS
    .word UNNEST

    ; ( cfa -- pfa )
    .colonword ">BODY",TOBODY
    .word CELLPLUS
    .word UNNEST

    ; ( vcfa -- voc )
    .colonword "VCFA>VOC",VCFATOVOC
    .word TWO,CELLSPLUS
    .word UNNEST

    ; ( voc -- vcfa )
    .colonword "VOC>VCFA",VOCTOVCFA
    .word TWO,CELLSMINUS
    .word UNNEST

    ; ( vlink -- voc )
    .colonword "VLINK>VOC",VLINKTOVOC
    .word CELLPLUS
    .word UNNEST

    ; ( voc -- vlink )
    .colonword "VOC>VLINK",VOCTOVLINK
    .word CELLMINUS
    .word UNNEST

;*****************************************************************************
; INTERPRET portion of QUERY-INTERPRET
;*****************************************************************************

    .deferword "SAVE-SRC",SAVESRC,NOOP
    .deferword "?UNSAVE-SRC",QUNSAVESRC,NOOP

    .colonword "(INTERPRET)",PINTERPRETP
    .word PBEGINP
@L1:
        .word BL,WORD,DUP,CFETCH
    .word PWHILEP,@L2
        .word SAVESRC,FIND,QDUP
        .word QBRANCH,@L3
            .word STATE,FETCH,EQUAL
            .word QBRANCH,@L4
                .word COMPILECOMMA
            .word BRANCH,@L5
@L4:
                .word EXECUTE,QSTACK
            .word PTHENP
@L5:
        .word BRANCH,@L6
@L3:
            .word NUMBER,NUMBERCOMMA
        .word PTHENP
@L6:
        .word QUNSAVESRC
    .word PREPEATP,@L1
@L2:
    .word DROP
    .word UNNEST

    .deferword "INTERPRET",INTERPRET,PINTERPRETP

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
    bcc @done
    inc DP+3
@done:
    jmp NEXT

    .deferword "COMPILE,",COMPILECOMMA,APPCOMPILECOMMA

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
    bcc @skip
    inc DP+3
    clc
@skip:
    lda IP
    adc #2
    sta IP
    bcc @done
    inc IP+1
@done:
    jmp NEXT

    .deferword "COMPILE",COMPILE,APPCOMPILE

    .colonword "APP-,",APPCOMMA
    .word HERE,STORE,TWO,DP,PLUSSTORE
    .word UNNEST

    .deferword ",",COMMA,APPCOMMA

    .colonword "APP-C,",APPCCOMMA
    .word HERE,CSTORE,DP,INCR
    .word UNNEST

    .deferword "C,",CCOMMA,APPCCOMMA

    .colonword "LINK,",LINKCOMMA
    .word HERE,OVER,FETCH,COMMA,SWAP,STORE
    .word UNNEST

    .colonword "(ALLOT)",PALLOTP
    .word DP,PLUSSTORE
    .word UNNEST

    .deferword "ALLOT",ALLOT,PALLOTP

    .deferword "ALIGN",ALIGN,NOOP

    .colonword ",\"",COMMAQUOTE
    .word LIT,'"',PARSE,HERE,TOR,DUP,CCOMMA
    .word DUP,ALLOT,FROMR,ONEPLUS,SWAP,MOVE
    .word UNNEST

    .icolonword "LITERAL",LITERAL
    .word COMPILE,LIT,COMMA
    .word UNNEST

    .varword "TEMP$",TEMPSTRING
    .res 256,0

    .deferword "NEW$",NEWSTRING,TEMPSTRING

    .varword "STATE",STATE
    .word 0

    .icolonword "S\"",SQUOTE
    .word STATE,FETCH
    .word QBRANCH,@else
        .word COMPILE,PSQUOTEP,COMMAQUOTE
    .word BRANCH,@then
@else:
        .word LIT,'"',PARSE,NEWSTRING,DUPTOR,PLACE,FROMR,COUNT
    .word PTHENP
@then:
    .word UNNEST

    .ideferword "(",PAREN,UNDERPAREN

    .icolonword "\\",BACKSLASH  ;"
    .word SOURCE,TOIN,STORE,DROP
    .word UNNEST

    .colonword "?STACK",QSTACK
    .word DEPTH,ZLESS,THROWSTACKUNDER,QTHROW
    .word UNNEST

    .colonword "(NUMBER,)",PNUMBERCOMMAP
    .word QDOUBLE,ZEQUAL
    .word QBRANCH,@then1
        .word DROP
    .word PTHENP
@then1:
    .word STATE,FETCH
    .word QBRANCH,@then2
        .word QDOUBLE
        .word QBRANCH,@then3
            .word SWAP,LITERAL
        .word PTHENP
@then3:
        .word LITERAL
    .word PTHENP
@then2:
    .word UNNEST

    .deferword "NUMBER,",NUMBERCOMMA,PNUMBERCOMMAP

    .colonword "([)",PLBRACKETP
    .word STATE,OFF
    .word UNNEST

    .ideferword "[",LBRACKET,PLBRACKETP

    .colonword "(])",PRBRACKETP
    .word STATE,ON
    .word UNNEST

    .deferword "]",RBRACKET,PRBRACKETP

    .colonword "?MEMCHK",QMEMCHK
    .word ZMAX,HERE,PLUS,TOPOFMEM,ONEMINUS,UGREATER
    .word THROWOUTOFMEM,QTHROW
    .word UNNEST

    .varword "LAST",LAST
    .word COLDNFA

    .varword "DEFER-LIST",DEFERLIST
    .word .ident (.sprintf("PD%08X",PREVDEFER))

    .colonword "(HIDE)",PHIDEP
    .word LAST,FETCH,NTOLINK,FETCH,CURRENT,FETCH,STORE
    .word UNNEST

    .deferword "HIDE",HIDE,PHIDEP

    .colonword "(REVEAL)",PREVEALP
    .word LAST,FETCH,CURRENT,FETCH,STORE
    .word UNNEST

    .deferword "REVEAL",REVEAL,PREVEALP

    .colonword "\"NAME,",QUOTENAMECOMMA
    .word MAXNAMECHARS,MIN,DUP,ZEQUAL,THROWNAMEREQD,QTHROW
    .word CAPS,FETCH
    .word QBRANCH,@then
        .word TWODUP,UPPER
    .word PTHENP
@then:
    .word HERE,TOR,DUP,CCOMMA,HERE,SWAP,DUP,ALLOT,MOVE
    .word RFETCH,COMMA,CURRENT,FETCH,FETCH,COMMA
    .word FROMR,LAST,STORE
    .word UNNEST

    .colonword "(\"HEADER)",PQUOTEHEADERP
    .word QUOTENAMECOMMA,LAST,FETCH,CURRENT,FETCH,STORE
    .word UNNEST

    .deferword "\"HEADER",QUOTEHEADER,PQUOTEHEADERP

    .colonword "(HEADER)",PHEADERP
    .word LIT,100,QMEMCHK,BL,WORD,COUNT,QUOTEHEADER
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

    .icolonword ";",SEMICOLON
    .word QCOMP,QCSP,REVEAL
    .word COMPILE,UNNEST,LBRACKET,DOSEMICHAIN
    .word UNNEST

    .colonword "CREATE",CREATE
    .word HEADER,DOVARCON,COMMA
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

    .colonword "VARIABLE",VARIABLE
    .word CREATE,ZERO,COMMA
    .word UNNEST

    .colonword "CONSTANT",CONSTANT
    .word HEADER,DOCONCON,COMMA,COMMA
    .word UNNEST

    .constword "IMMEDBIT",IMMEDBIT,$80

    .colonword "IMMEDIATE",IMMEDIATE
    .word IMMEDBIT,LAST,FETCH,TOGGLE
    .word UNNEST

    .colonword "?PAIRS",QPAIRS
    .word DOXOR,THROWMISMATCH,QTHROW
    .word UNNEST

    .colonword ">MARK",FWDMARK
    .word HERE,ZERO,COMMA
    .word UNNEST

    .colonword ">RESOLVE",FWDRESOLVE
    .word HERE,SWAP,STORE
    .word UNNEST

    .colonword "<MARK",BACKMARK
    .word HERE
    .word UNNEST

    .colonword "<RESOLVE",BACKRESOLVE
    .word COMMA
    .word UNNEST

    .icolonword "AHEAD",AHEAD
    .word QCOMP,COMPILE,BRANCH,FWDMARK,TWO
    .word UNNEST

    .icolonword "IF",IF
    .word QCOMP,COMPILE,QBRANCH,FWDMARK,TWO
    .word UNNEST

    .icolonword "THEN",THEN
    .word QCOMP,TWO,QPAIRS,COMPILE,PTHENP,FWDRESOLVE
    .word UNNEST

    .icolonword "ENDIF",ENDIF
    .word QCOMP,TWO,QPAIRS,COMPILE,PTHENP,FWDRESOLVE
    .word UNNEST

    .icolonword "ELSE",ELSE
    .word QCOMP,TWO,QPAIRS,COMPILE,BRANCH,FWDMARK,SWAP,FWDRESOLVE,TWO
    .word UNNEST

    .icolonword "BEGIN",BEGIN
    .word QCOMP,COMPILE,PBEGINP,BACKMARK,ONE
    .word UNNEST

    .icolonword "UNTIL",UNTIL
    .word QCOMP,ONE,QPAIRS,COMPILE,PUNTILP,BACKRESOLVE
    .word UNNEST

    .icolonword "WHILE",WHILE
    .word QCOMP,COMPILE,PWHILEP,FWDMARK,TWO,TWOSWAP
    .word UNNEST

    .icolonword "REPEAT",REPEAT
    .word QCOMP,ONE,QPAIRS,COMPILE,PREPEATP,BACKRESOLVE,TWO,QPAIRS,FWDRESOLVE
    .word UNNEST

    .icolonword "DO",DO
    .word QCOMP,COMPILE,PDOP,FWDMARK,THREE
    .word UNNEST

    .icolonword "LOOP",LOOP
    .word QCOMP,THREE,QPAIRS,COMPILE,PLOOPP,DUP,CELLPLUS,BACKRESOLVE,FWDRESOLVE
    .word UNNEST

    .icolonword "+LOOP",PLUSLOOP
    .word QCOMP,THREE,QPAIRS,COMPILE,PPLUSLOOPP,DUP,CELLPLUS,BACKRESOLVE,FWDRESOLVE
    .word UNNEST

    .icolonword "EXIT",EXIT
    .word QCOMP,COMPILE,PEXITP
    .word UNNEST

    .icolonword "RECURSE",RECURSE
    .word QCOMP,LAST,FETCH,NAMETO,COMPILECOMMA
    .word UNNEST

    .colonword ">IS",TOIS
    .word CELLPLUS
    .word UNNEST

    .colonword "(IS)",PISP
    .word FETCHPIPP,TOIS,STORE
    .word UNNEST

    .colonword "?IS",QIS
    .word DUP,FETCH,DODEFERCON,NOTEQUAL,THROWNOTDEFER,QTHROW
    .word UNNEST

    .icolonword "IS",IS
    .word STATE,FETCH
    .word QBRANCH,@else
        .word COMPILE,PISP,TICK,QIS,COMMA
    .word BRANCH,@then
@else:
        .word TICK,QIS,TOIS,STORE
    .word PTHENP
@then:
    .word UNNEST

    .colonword "CALL,",CALLCOMMA
    .word LIT,$20EA,COMMA,COMMA     ; $20EA => EA 20 => NOP JSR
    .word UNNEST

    .colonword "(;CODE)",PSEMICODEP
    .word FROMR,LAST,FETCH,NAMETO,STORE
    .word UNNEST

    .icolonword "DOES>",DOES
    .word QCOMP,COMPILE,PSEMICODEP,DODOESCON,CALLCOMMA
    .word UNNEST

    .icolonword "POSTPONE",POSTPONE
    .word DEFINED,DUP,ZEQUAL,QMISSING,ZLESS
    .word QBRANCH,@then
        .word COMPILE,COMPILE
    .word PTHENP
@then:
    .word COMPILECOMMA
    .word UNNEST

    .icolonword "[']",BTICKB
    .word TICK,LITERAL
    .word UNNEST

    .colonword "CHAR",CHAR
    .word BL,WORD,ONEPLUS,CFETCH
    .word UNNEST

    .icolonword "[CHAR]",BCHARB
    .word CHAR,LITERAL
    .word UNNEST

    .icolonword ".\"",DOTQUOTE
    .word COMPILE,PDOTQUOTEP,COMMAQUOTE
    .word UNNEST

    .colonword "DEFER",DEFER
    .word HEADER,DODEFERCON,COMMA,COMPILE,NOOP,DEFERLIST,LINKCOMMA,COMPILE,NOOP
    .word UNNEST

    .colonword "(ABORT\")",PABORTQUOTEP
    .word PPQUOTEPP,SWAP
    .word QBRANCH,@then
        .word MSG,STORE,THROWABORTQ,THROW
    .word PTHENP
@then:
    .word DROP
    .word UNNEST

    .colonword "ABORT",ABORT
    .word THROWABORT,THROW
    .word UNNEST

    .icolonword "ABORT\"",ABORTQUOTE
    .word COMPILE,PABORTQUOTEP,COMMAQUOTE
    .word UNNEST

    .colonword "?TO-CHECK",QTOCHECK
    .word DUP,FETCH,TOR,TOBODY,DUP,CELLPLUS,FETCH,LIT,$FFFF,EQUAL
    ; PATCH -----------------------
    .word RFETCH,DOVALUECON,NOTEQUAL
    .word FROMR,DO2VALUECON,NOTEQUAL,DOAND,DOOR
    ; END PATCH -------------------
    ;.word RFETCH,DOCONCON,EQUAL,DOOR
    ;.word RFETCH,DOCOLONCON,EQUAL,DOOR
    ;.word RFETCH,DOVARCON,EQUAL,DOOR
    ;.word FROMR,DODEFERCON,EQUAL,DOOR
    .word THROWNOTVALUE,QTHROW
    .word UNNEST

    .colonword "TOCOMPEXEC",TOCOMPEXEC
    .word TICK,QTOCHECK,PLUS,STATE,FETCH
    .word QBRANCH,@else
        .word COMPILECOMMA
    .word BRANCH,@then
@else:
        .word EXECUTE
    .word PTHENP
@then:
    .word UNNEST

    .icolonword "TO",TO
    .word CELL,TOCOMPEXEC
    .word UNNEST

    .icolonword "+TO",PLUSTO
    .word TWO,CELLS,TOCOMPEXEC
    .word UNNEST

    .deferword "ALIGNED",ALIGNED,NOOP

    .colonword "_SAVE-INPUT",USAVEINPUT
    .word TOIN,FETCH,SOURCEPOSITION,SOURCEID,SOURCE,FIVE
    .word UNNEST

    .colonword "_RESTORE-INPUT",URESTOREINPUT
    .word FIVE,QPAIRS
    .word PSOURCEP,TWOSTORE,SOURCEID+4,SOURCEPOSITION+4,TOIN,STORE,ZERO
    .word UNNEST

    .deferword "SAVE-INPUT",SAVEINPUT,USAVEINPUT
    .deferword "RESTORE-INPUT",RESTOREINPUT,URESTOREINPUT

    .colonword "EVALUATE",EVALUATE
    .word SAVEINPUT,PSAVEINPUTP
    .word PSOURCEP,TWOSTORE,TOIN,OFF,LIT,$FFFF,SOURCEID+4,ZERO,SOURCEPOSITION+4
    .word LIT,INTERPRET,CATCH,DUP
    .word QBRANCH,@then
        .word TOIN,FETCH,SWAP
    .word PTHENP
@then:
    .word PRESTOREINPUTP,RESTOREINPUT,DROP,DUP
    .word QBRANCH,@then2
        .word SWAP,TOIN,STORE
    .word PTHENP
@then2:
    .word THROW
    .word UNNEST

    .colonword ":NONAME",NONAME
    .word HERE,DOCOLONCON,COMMA,STORECSP,RBRACKET
    .word UNNEST

    .icolonword "?DO",QDO
    .word QCOMP,COMPILE,PQDOP,FWDMARK,THREE
    .word UNNEST

    .icolonword "AGAIN",AGAIN
    .word QCOMP,ONE,QPAIRS,COMPILE,PAGAINP,BACKRESOLVE
    .word UNNEST

    .icolonword "CASE",CASE
    .word QCOMP,COMPILE,PCASEP,ZERO
    .word UNNEST

    .icolonword "OF",OF
    .word QCOMP,COMPILE,POFP,FWDMARK,FOUR
    .word UNNEST

    .icolonword "ENDOF",ENDOF
    .word QCOMP,FOUR,QPAIRS,COMPILE,PENDOFP,FWDMARK,SWAP,FWDRESOLVE,FIVE
    .word UNNEST

    .icolonword "ENDCASE",ENDCASE
    .word QCOMP,COMPILE,PENDCASEP
    .word PBEGINP
@loop:
        .word QDUP
    .word PWHILEP,@done
        .word FIVE,QPAIRS,FWDRESOLVE
    .word PREPEATP,@loop
@done:
    .word UNNEST

    .colonword "VALUE",VALUE
    .word HEADER,DOVALUECON,COMMA,COMMA
    .word DOVALUESTORECON,COMMA,DOVALUEPSTORECON,COMMA
    .word UNNEST

    .icolonword "[COMPILE]",BCOMPILEB
    .word TICK,COMPILECOMMA
    .word UNNEST

    .icolonword "?EXIT",QEXIT
    .word QCOMP,COMPILE,QBRANCH,FWDMARK
    .word EXIT,COMPILE,PTHENP,FWDRESOLVE
    .word UNNEST

;*****************************************************************************
; Core Extension Words
;*****************************************************************************

    .colonword "ERASE",ERASE
    .word ZERO,FILL
    .word UNNEST

    .colonword "D.R",DDOTR
    .word TOR,PDDOTP,FROMR,OVER,MINUS,SPACES,TYPE
    .word UNNEST

    .colonword ".R",DOTR
    .word TOR,STOD,FROMR,DDOTR
    .word UNNEST

    .colonword "U.R",UDOTR
    .word ZERO,SWAP,DDOTR
    .word UNNEST

    .colonword "H.",HDOT
    .word BASE,FETCH,SWAP,HEX,UDOT,BASE,STORE
    .word UNNEST

    .colonword "?",QUESTION
    .word FETCH,DOT
    .word UNNEST

    .colonword "H.R",HDOTR
    .word BASE,FETCH,TOR,HEX,UDOTR,FROMR,BASE,STORE
    .word UNNEST

    .colonword "H.N",HDOTN
    .word BASE,FETCH,TOR,HEX,TOR
    .word ZERO,BEGNUMBER,FROMR,ZERO
    .word PQDOP,@done
@loop:
        .word NUMBERSIGN
    .word PLOOPP,@loop
@done:
    .word NUMBEREND,TYPE
    .word FROMR,BASE,STORE
    .word UNNEST

    .colonword "H.2",HDOT2
    .word TWO,HDOTN
    .word UNNEST

    .colonword "H.4",HDOT4
    .word FOUR,HDOTN
    .word UNNEST

    .colonword "H.8",HDOT8
    .word LIT,8,HDOTN
    .word UNNEST

    .colonword "0<>",ZNOTEQUAL
    .word ZEQUAL,INVERT
    .word UNNEST

    .colonword "HOLD$",HOLDSTRING
    .word HLD,FETCH,OVER,MINUS,DUP,HLD,STORE,SWAP,MOVE
    .word UNNEST

    .colonword "PARSE-NAME",PARSENAME
    .word BL,WORD,COUNT
    .word UNNEST

    .colonword "BUFFER:",BUFFERCOLON
    .word CREATE,ALLOT
    .word UNNEST

    .colonword "DEFER!",DEFERSTORE
    .word QIS,TOIS,STORE
    .word UNNEST

    .colonword "DEFER@",DEFERFETCH
    .word QIS,TOIS,FETCH
    .word UNNEST

    .icolonword "ACTION-OF",ACTIONOF
    .word TICK,QIS,TOIS,STATE,FETCH
    .word QBRANCH,@else
        .word LITERAL,COMPILE,FETCH
    .word BRANCH,@then
@else:
        .word FETCH
    .word PTHENP
@then:
    .word UNNEST

    .icolonword "LOCALALLOC",LOCALALLOC
    .word COMPILE,PLOCALALLOCP
    .word UNNEST

    .colonword "C+PLACE",CPLUSPLACE
    .word DUP,CINCR,COUNT,PLUS,ONEMINUS,STORE
    .word UNNEST

    .icolonword "C\"",CQUOTE
    .word STATE,FETCH
    .word QBRANCH,@else
        .word COMPILE,PCQUOTEP,COMMAQUOTE
    .word BRANCH,@then
@else:
        .word LIT,39,WORD,NEWSTRING,DUPTOR,OVER,CFETCH,ONEPLUS,MOVE,FROMR
    .word PTHENP
@then:
    .word UNNEST

    .colonword "UNUSED",UNUSED
    .word TOPOFMEM,HERE,MINUS
    .word UNNEST

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
    .word PDOTQUOTEP
    .ptext " ok"
    .word DEPTH,DOTSMAX,FETCH,MIN,ZERO
    .word PQDOP,@L1
@L2:
        .word LIT,'.',EMIT
    .word PLOOPP,@L2
@L1:
    .word UNNEST

    .deferword ".OK",DOTOK,PDOTOKP
    .deferword ".PROMPT",DOTPROMPT,PDOTPROMPTP
    .deferword ".NOTOK",DOTNOTOK,NOOP

    .varword "(EXCEPTION-SOURCE)",PEXCEPTIONSOURCEP
    .word 0
    .word 0

    .colonword "?TYPE",QTYPE
    .word COUNT,QDUP
    .word QBRANCH,@else
        .word TYPE,SPACE
    .word BRANCH,@then
@else:
        .word DROP
    .word PTHENP
@then:
    .word UNNEST

    .colonword "(TYPEMSG)",PTYPEMSGP
    .word QLOADING,FETCH
    .word QBRANCH,@L1
        .word PEXCEPTIONSOURCEP
    .word BRANCH,@L2
@L1:
        .word PSOURCEP
    .word PTHENP
@L2:
    .word CR,TWOFETCH,TYPE,CR,TOIN,FETCH,DUP,QLOADING,FETCH
    .word QBRANCH,@L3
        .word PEXCEPTIONSOURCEP
    .word BRANCH,@L4
@L3:
        .word PSOURCEP
    .word PTHENP
@L4:
    .word FETCH,LESS,PLUS,POCKET,CFETCH,DUPTOR,MINUS,ONEMINUS,SPACES
    .word FROMR,ONE,MAX,ZERO
    .word PQDOP,@L5
@L6:
        .word LIT,'^',EMIT
    .word PLOOPP,@L6
@L5:
    .word BASE,FETCH,TOR,DECIMAL,CR,TYPE
    .word LIT,'(',EMIT
    .word DUP,LASTERROR+4,STOD,PDDOTP,TYPE
    .word PDOTQUOTEP
    .ptext "): "
    .word POCKET,QTYPE,THROWMSGS
    .word PBEGINP
@L7:
        .word FETCH,QDUP
        .word PWHILEP,@L8
        .word DUP,CELLPLUS,FETCH,LASTERROR,EQUAL
        .word QBRANCH,@L9
            .word TWO,CELLSPLUS,QTYPE,NULLMSG,MSG,STORE,PTRNULL
        .word PTHENP
@L9:
    .word PREPEATP,@L7
@L8:
    .word MSG,FETCH,QTYPE,NULLMSG,MSG,STORE,QLOADING,FETCH
    .word QBRANCH,@LA
        .word PDOTQUOTEP
        .ptext "in file "
        .word LOADFILE,FETCH,CELLPLUS,QTYPE
        .word PDOTQUOTEP
        .ptext "at line "
        .word LOADLINE,FETCH,UDOT
    .word PTHENP
@LA:
    .word FROMR,BASE,STORE
    .word UNNEST

    .deferword "EDIT-ERROR",EDITERROR,NOOP

    .colonword "(MESSAGE)",PMESSAGEP
    .word DUP,ONEPLUS
    .word QBRANCH,@L1
        .word PSQUOTEP
        .ptext "Error "
        .word PTYPEMSGP
        .word QLOADING,FETCH
        .word QBRANCH,@L3
            .word EDITERROR
        .word PTHENP
@L3:
    .word BRANCH,@L2
@L1:
        .word DROP
    .word PTHENP
@L2:
    .word UNNEST

    .deferword "MESSAGE",MESSAGE,PMESSAGEP

    .colonword "(RESET-STACKS)",PRESETSTACKSP
    .word SP0,FETCH,SPSTORE
    .word UNNEST

    .deferword "RESET-STACKS",RESETSTACKS,PRESETSTACKSP

    .colonword "QUERY-INTERPRET",QUERYINTERPRET
    .word QUERY,SPACE,INTERPRET
    .word UNNEST

    .colonword "(QUIT)",PQUITP
    .word RP0,FETCH,RPSTORE
    .word PBEGINP
@L1:
        .word LBRACKET,QLOADING,OFF
        .word PBEGINP
@L2:
            .word DOTPROMPT,LIT,QUERYINTERPRET,CATCH,QDUP,ZEQUAL
        .word PWHILEP,@L3
            .word STATE,FETCH,ZEQUAL
            .word QBRANCH,@L4
                .word DOTOK
            .word PTHENP
@L4:
        .word PREPEATP,@L2
@L3:
        .word DUP,ONEPLUS
        .word QBRANCH,@L5
            .word MESSAGE
        .word PTHENP
@L5:
        .word DOTNOTOK,RESETSTACKS
    .word PAGAINP,@L1
    .word UNNEST

    .deferword "QUIT",QUIT,PQUITP

;*****************************************************************************
; Forth File I/O Words
;*****************************************************************************

    .constword "R/O",READONLY,FILEIO_MODE_READ
    .constword "W/O",WRITEONLY,FILEIO_MODE_WRITE
    .constword "R/W",READWRITE,FILEIO_MODE_RW
    .constword "BIN-MODE",BINMODE,FILEIO_MODE_BIN

    ;.deferword "BIN",BIN,NOOP
    ; ( mode -- mode' )
    .colonword "BIN",BIN
    .word BINMODE,DOOR
    .word UNNEST

    .varword "_FILE-NAME_",UFILENAMEU
    .res 256

    ; ( addr len mode -- fd ior )
    .codeword "(OPEN-FILE)",POPENFILEP
    jsr dpopToGP3           ; file mode
    jsr dpopToGP2           ; name length
    jsr dpopToGP0           ; filename
    lda #<(UFILENAMEU+2)
    sta GP1
    lda #>(UFILENAMEU+2)
    sta GP1+1
    jsr memMove
    clc
    lda GP2
    adc #<(UFILENAMEU+2)
    sta GP0
    lda #0
    adc #>(UFILENAMEU+2)
    sta GP0+1
    lda #0
    sta (GP0)
    lda GP3
    ldx #<(UFILENAMEU+2)
    ldy #>(UFILENAMEU+2)
    jsr FILEOPEN
    pha
    ldx #0
    jsr dpush
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( fd -- ior )
    .codeword "(CLOSE-FILE)",PCLOSEFILEP
    jsr dpop
    jsr FILECLOSE
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( addr len fd -- len' ior )
    .codeword "(READ-FILE)",PREADFILEP
    jsr dpopToGP0       ; fd
    jsr dpopToGP1       ; count
    jsr dpopToGP2       ; target
    stz GP3             ; actual count
    stz GP3+1
@loop:
    lda GP1
    ora GP1+1
    beq @done
    lda GP0
    jsr FILEREAD
    bcc @storeByte
    lda FILEIO::STATUS
    cmp #FILEIO_STATUS_EOF
    beq @done
    lda GP3
    ldx GP3+1
    jsr dpush
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT
@storeByte:
    sta (GP2)
    inc GP2
    bne @skip1
    inc GP2+1
@skip1:
    inc GP3
    bne @skip2
    inc GP3+1
@skip2:
    sec
    lda GP1
    sbc #1
    sta GP1
    bcs @skip3
    dec GP1+1
@skip3:
    bra @loop
@done:
    lda GP3
    ldx GP3+1
    jsr dpush
    lda #0
    tax
    jmp PUSHNEXT

    ; ( addr len fd -- len' flag ior )
    .codeword "(READ-LINE)",PREADLINEP
    jsr dpopToGP0       ; fd
    jsr dpopToGP1       ; count
    jsr dpopToGP2       ; target
    stz GP3             ; actual count
    stz GP3+1
@loop:
    lda GP1
    ora GP1+1
    beq @done
    lda GP0
    jsr FILEREAD
    bcc @checkEOL
    lda FILEIO::STATUS
    cmp #FILEIO_STATUS_EOF
    beq @eof
    lda GP3
    ldx GP3+1
    jsr dpush
    lda #0
    tax
    jsr dpush
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT
@checkEOL:
    cmp #$0A
    beq @done
@storeByte:
    sta (GP2)
    inc GP2
    bne @skip1
    inc GP2+1
@skip1:
    inc GP3
    bne @skip2
    inc GP3+1
@skip2:
    sec
    lda GP1
    sbc #1
    sta GP1
    bcs @skip3
    dec GP1+1
@skip3:
    bra @loop
@eof:
    lda GP3
    ldx GP3+1
    jsr dpush
    lda #0
    tax
    jsr dpush
    lda #0
    tax
    jmp PUSHNEXT
@done:
    lda GP3
    ldx GP3+1
    jsr dpush
    lda #$FF
    tax
    jsr dpush
    lda #0
    tax
    jmp PUSHNEXT

    ; ( addr len fd -- ior )
    .codeword "(WRITE-FILE)",PWRITEFILEP
    jsr dpopToGP0       ; fd
    jsr dpopToGP1       ; count
    jsr dpopToGP2       ; source
@nextPage:
    lda GP1+1
    beq @lastPage
    ldy #0
@writeChar:
    ldx GP0
    lda (GP2),y
    jsr KFILEWRITE
    bcs @error
    iny
    bne @writeChar
    inc GP2+1
    dec GP1+1
    bra @nextPage
@lastPage:
    lda GP1
    beq @done
    ldy #0
@lpWriteChar:
    pha
    ldx GP0
    lda (GP2),y
    jsr KFILEWRITE
    bcs @error
    iny
    pla
    dec a
    bne @lpWriteChar
@done:
    lda #0
    tax
    jmp PUSHNEXT
@error:
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( addr len fd -- ior )
    .codeword "(WRITE-LINE)",PWRITELINEP
    jsr dpopToGP0       ; fd
    jsr dpopToGP1       ; count
    jsr dpopToGP2       ; source
@nextPage:
    lda GP1+1
    beq @lastPage
    ldy #0
@writeChar:
    ldx GP0
    lda (GP2),y
    jsr KFILEWRITE
    bcs @error
    iny
    bne @writeChar
    dec GP1+1
    bra @nextPage
@lastPage:
    lda GP1
    beq @done
    ldy #0
@lpWriteChar:
    pha
    ldx GP0
    lda (GP2),y
    jsr KFILEWRITE
    bcs @error
    iny
    pla
    dec a
    bne @lpWriteChar
@done:
    lda #$0A
    ldx GP0
    jsr KFILEWRITE
    bcs @error
    lda #0
    tax
    jmp PUSHNEXT
@error:
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( addr len -- ior )
    .codeword "(DELETE-FILE)",PDELETEFILEP
    jsr dpopToGP2           ; name length
    jsr dpopToGP0           ; filename
    lda #<(UFILENAMEU+2)
    sta GP1
    lda #>(UFILENAMEU+2)
    sta GP1+1
    jsr memMove
    clc
    lda GP2
    adc #<(UFILENAMEU+2)
    sta GP0
    lda #0
    adc #>(UFILENAMEU+2)
    sta GP0+1
    lda #0
    sta (GP0)
    lda GP3
    ldx #<(UFILENAMEU+2)
    ldy #>(UFILENAMEU+2)
    jsr FILEDELETE
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( fd -- ud ior )
    .codeword "(FILE-POSITION)",PFILEPOSITIONP
    jsr dpop
    sta FILEIO::FD
    lda #FILEIO_FILEPOS
    sta FILEIO::CMD
    jsr FILEEXECIO
    lda FILEIO::DATA_LO
    ldx FILEIO::DATA_HI
    jsr dpush
    lda FILEIO::DATA_LO2
    ldx FILEIO::DATA_HI2
    jsr dpush
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( fd -- ud ior )
    .codeword "(FILE-SIZE)",PFILESIZEP
    jsr dpop
    sta FILEIO::FD
    lda #FILEIO_FILESIZ
    sta FILEIO::CMD
    jsr FILEEXECIO
    lda FILEIO::DATA_LO
    ldx FILEIO::DATA_HI
    jsr dpush
    lda FILEIO::DATA_LO2
    ldx FILEIO::DATA_HI2
    jsr dpush
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( caddr len -- status ior )
    .colonword "(FILE-STATUS)",PFILESTATUSP
    .word TWODROP,ZERO,ZERO
    .word UNNEST

    ; ( fd -- ior )
    .codeword "(FILE-FLUSH)",PFILEFLUSHP
    jsr dpop
    jsr FILEFLUSH
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( caddr1 len1 caddr2 len2 -- ior )
    .colonword "(FILE-RENAME)",PFILERENAMEP
    .word TWODROP,TWODROP,FILEIO_STATUS_ERR
    .word UNNEST

    ; ( ud fd -- ior )
    .colonword "(REPOSITION-FILE)",PREPOSITIONFILEP
    jsr dpop
    sta FILEIO::FD
    jsr dpop
    sta FILEIO::DATA_LO2
    stx FILEIO::DATA_HI2
    jsr dpop
    sta FILEIO::DATA_LO
    stx FILEIO::DATA_HI
    lda #FILEIO_SEEK
    jsr FILEEXECIO
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    ; ( ud fd -- ior )
    .colonword "(RESIZE-FILE)",PRESIZEFILEP
    jsr dpop
    sta FILEIO::FD
    jsr dpop
    sta FILEIO::DATA_LO2
    stx FILEIO::DATA_HI2
    jsr dpop
    sta FILEIO::DATA_LO
    stx FILEIO::DATA_HI
    lda #FILEIO_RESIZE
    jsr FILEEXECIO
    lda FILEIO::STATUS
    ldx #0
    jmp PUSHNEXT

    .deferword "OPEN-FILE",OPENFILE,POPENFILEP
    .deferword "CREATE-FILE",CREATEFILE,PCLOSEFILEP
    .deferword "CLOSE-FILE",CLOSEFILE,PCLOSEFILEP
    .deferword "READ-FILE",READFILE,PREADFILEP
    .deferword "READ-LINE",READLINE,PREADLINEP
    .deferword "WRITE-FILE",WRITEFILE,PWRITEFILEP
    .deferword "WRITE-LINE",WRITELINE,PWRITELINEP
    .deferword "DELETE-FILE",DELETEFILE,PDELETEFILEP
    .deferword "FILE-POSITION",FILEPOSITION,PFILEPOSITIONP
    .deferword "FILE-SIZE",FILESIZE,PFILESIZEP
    .deferword "FILE-STATUS",FFILESTATUS,PFILESTATUSP
    .deferword "FILE-FLUSH",FFILEFLUSH,PFILEFLUSHP
    .deferword "FILE-RENAME",FILERENAME,PFILERENAMEP
    .deferword "REPOSITION-FILE",REPOSITIONFILE,PREPOSITIONFILEP
    .deferword "RESIZE-FILE",RESIZEFILE,PRESIZEFILEP

    .varword "OPENBUF",OPENBUF
    .res MAXBUFSIZE,0

    .varword "CUR-FILE",CURFILE
    .res MAXBUFSIZE,0

    .varword "CUR-LINE",CURLINE
    .word 0

    .colonword "LINKFILE",LINKFILE
    .word QLOADING,FETCH
    .word QBRANCH,@else
        .word LOADFILE,LINKCOMMA
        .word COUNT,HERE,PLACE,HERE,CFETCH,ONEPLUS,ALLOT
    .word BRANCH,@then
@else:
        .word DROP
    .word PTHENP
@then:
    .word UNNEST

    .varword "START-LINE",STARTLINE
    .word 0

    .varword "ECHO",ECHO
    .word 0

    .colonword "?.REFILL",QDOTREFILL
    .word ECHO,FETCH
    .word QBRANCH,@then
        .word CR,SOURCE,TYPE
    .word PTHENP
@then:
    .word UNNEST

    .deferword ".REFILL",DOTREFILL,QDOTREFILL

    .valueword "INCLUDING?",INCLUDINGQ,0

    .valueword "LEN-PREV",LENPREV,0

    .colonword "REFILL",REFILL
    .word SOURCEID,QDUP
    .word QBRANCH,@L4
        .word ONEPLUS
        .word QBRANCH,@L3
            .word ONE,LOADLINE,PLUSSTORE,TIB,DUP,MAXSTRING
            .word LENPREV,SOURCEPOSITION+6
            .word SOURCEID,READLINE,THROWFILEREADFAIL,QTHROW
            .word QBRANCH,@L1
                .word DUP,TWO,PLUS,LENPREV+4,TWODUP
                .word PEXCEPTIONSOURCEP,TWOSTORE,PSOURCEP,TWOSTORE
                .word TOIN,OFF,DOTREFILL,TRUECON,PEXITP
            .word BRANCH,@L2
@L1:
                .word ZERO,LENPREV+4
            .word PTHENP
@L2:        .word TWODROP
        .word PTHENP
@L3:
        .word FALSECON,PEXITP
    .word PTHENP
@L4:
    .word CR,QUERY,TRUECON
    .word UNNEST

    .colonword ">LINE",TOLINE
    .word ONEMINUS,ZERO,MAX,QDUP
    .word QBRANCH,@L3
        .word ZERO
        .word PDOP,@L2
@L1:
            .word REFILL,DROP
        .word PLOOPP,@L1
@L2:
    .word PTHENP
@L3:
    .word UNNEST

    .deferword "STACK-CHECK",STACKCHECK,NOOP

    .colonword "DO-INCLUDE",DOINCLUDE
    .word INCLUDINGQ,TOR,TRUECON,INCLUDINGQ+4
    .word STARTLINE,FETCH,TOLINE,STARTLINE,OFF
    .word SOURCEPOSITION,TOR,ZERO,SOURCEPOSITION+4
    .word LENPREV,TOR,ZERO,LENPREV+4
    .word PBEGINP
@L1:
        .word REFILL,INCLUDINGQ,DOAND
    .word PWHILEP,@L2
        .word INTERPRET,STACKCHECK
    .word PREPEATP,@L1
@L2:
    .word FROMR,LENPREV+4,FROMR,SOURCEPOSITION+4,FROMR,INCLUDINGQ+4
    .word UNNEST

    .deferword "START-INCLUDE",STARTINCLUDE,NOOP
    .deferword "END-INCLUDE",ENDINCLUDE,NOOP

    .colonword "INCLUDE-FILE",INCLUDEFILE
    .word MAXSTRING,PLOCALALLOCP,TOR,TIB,RFETCH,MAXSTRING,MOVE
    .word LOADFILE,FETCH,CELLPLUS,TOR,QLOADING,FETCH,TOR
    .word LOADLINE,FETCH,TOR,TOIN,FETCH,TOR,SOURCEID,TOR
    .word SOURCEID+4,SOURCE,TWOTOR,SOURCEPOSITION,TOR
    .word QLOADING,ON,POCKET,LINKFILE,LOADLINE,OFF,ZERO,SOURCEPOSITION+4
    .word STARTINCLUDE
        .word LIT,DOINCLUDE,CATCH,SOURCEID,CLOSEFILE,DROP
        .word FROMR,SOURCEPOSITION+4,TWOFROMR,PSOURCEP,TWOSTORE,FROMR,SOURCEID+4
    .word ENDINCLUDE
    .word THROW
    .word FROMR,TOIN,STORE,FROMR,LOADLINE,STORE,FROMR,QLOADING,STORE
    .word FROMR,LINKFILE
    .word FROMR,TIB,MAXSTRING,MOVE,QLOADING,FETCH
    .word QBRANCH,@then
        .word LOADFILE,FETCH,CELLPLUS,COUNT,CLIPSTRING,CURFILE,PLACE
    .word PTHENP
@then:
    .word PLOCALFREEP
    .word UNNEST

    ; ( addr len -- fd f )
    .colonword "(\"OPEN)",PQUOTEOPENP
    .word OPENBUF,PLACE
    .word OPENBUF,COUNT,READWRITE,OPENFILE,DUP,ZEQUAL
    .word QBRANCH,@then
        .word OPENBUF,COUNT,CURFILE,PLACE
    .word PTHENP
@then:
    .word OPENBUF,COUNT,POCKET,PLACE
    .word UNNEST

    .deferword "\"OPEN",QUOTEOPEN,PQUOTEOPENP

    .colonword "INCLUDED",INCLUDED
    .word QUOTEOPEN,THROWFILENOTFOUND,QTHROW,INCLUDEFILE
    .word UNNEST

    .colonword "FLOAD",FLOAD
    .word BL,WORD,COUNT,INCLUDED
    .word UNNEST

    .colonword "FORTH-KERNEL-SIZE",FORTHKERNELSIZE
    .word DP,FETCH,FORTHKERNELSTART,MINUS
    .word UNNEST

    .colonword "FSAVE",FSAVE
    .word BL,WORD,COUNT
    .word WRITEONLY,BIN,OPENFILE,THROWFILECREATEFAIL,QTHROW,TOR
    .word FORTHKERNELSTART,HERE,STORE,HERE,LIT,2,RFETCH
    .word WRITEFILE,THROWFILEWRITEFAIL,QTHROW
    .word FORTHKERNELSTART,FORTHKERNELSIZE
    .word RFETCH,WRITEFILE,THROWFILEWRITEFAIL,QTHROW
    .word FROMR,CLOSEFILE,THROWFILECLOSEFAIL,QTHROW
    .word UNNEST


;*****************************************************************************
; Forth cold start initialization
;*****************************************************************************

    .deferword "WELCOME",WELCOME,NOOP

COLDNFA:
    .colonword "COLD",COLD
    .word WELCOME,QUIT
    .word UNNEST

;*****************************************************************************
; Initialize Forth for cold start
;*****************************************************************************
ICOLD:
    cld

    lda #<DSTACK_TOP
    sta SP
    sta SP0+2
    lda #>DSTACK_TOP
    sta SP+1
    sta SP0+3

    lda #<RSTACK_TOP
    sta RP
    sta RP0+2
    lda #>RSTACK_TOP
    sta RP+1
    sta RP0+3

    lda #<COLD
    sta CFA
    lda #>COLD
    sta CFA+1

    lda #10
    sta BASE+2
    stz BASE+3
    jmp EXEC

ENDOFKERNEL:
