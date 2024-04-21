;*****************************************************************************
; Some Macros
;*****************************************************************************

PREVHEADER	.set 0
.ident (.sprintf("PH%08X",PREVHEADER)) := PREVHEADER
PREVDEFER	.set 0
.ident (.sprintf("PD%08X",PREVDEFER))  := PREVDEFER

    .macro add arg1,arg2
    clc
    .ifblank arg2
        adc arg1
    .else
        adc arg1,arg2
    .endif
    .endmacro

    .macro sub arg1,arg2
    sec
    .ifblank arg2
        sbc arg1
    .else
        sbc arg1,arg2
    .endif
    .endmacro

    .macro ptext arg
    .byte .strlen(arg),arg
    .endmacro

    .macro iptext arg
    .byte .strlen(arg)|$80,arg
    .endmacro

    .macro inc16 addr
    .local skip
    inc addr
    bne skip
    inc addr+1
skip:
    .endmacro

    .macro dec16 addr
    .local skip
    sec
    lda addr
    sbc #1
    sta addr
    bcs skip
    dec addr+1
skip:
    .endmacro

    .macro wordheader name
.ident (.sprintf("PH%08X",PREVHEADER+1)):
    ptext name
    .word .ident (.sprintf("PH%08X",PREVHEADER+1))
    .word .ident (.sprintf("PH%08X",PREVHEADER))
    PREVHEADER .set PREVHEADER + 1
    .endmacro

    .macro iwordheader name
.ident (.sprintf("PH%08X",PREVHEADER+1)):
    iptext name
    .word .ident (.sprintf("PH%08X",PREVHEADER+1))
    .word .ident (.sprintf("PH%08X",PREVHEADER))
    PREVHEADER .set PREVHEADER + 1
    .endmacro

    .macro codeword name,label
    wordheader name
label:
    .word * + 2
    .endmacro

    .macro icodeword name,label
    iwordheader name
label:
    .word * + 2
    .endmacro

    .macro colonword name,label
    wordheader name
label:
    .word DOCOLON
    .endmacro

    .macro icolonword name,label
    iwordheader name
label:
    .word DOCOLON
    .endmacro

    .macro varword name,label
    wordheader name
label:
    .word DOVAR
    .endmacro

    .macro constword name,label,value
    wordheader name
label:
    .word DOCON
    .word value
    .endmacro

    .macro deferword name,label,value
    wordheader name
label:
    .word DODEFER
    .word value
.ident (.sprintf("PD%08X",PREVDEFER+1)):
    .word .ident (.sprintf("PD%08X",PREVDEFER))
    .word value
    PREVDEFER .set PREVDEFER + 1
    .endmacro

    .macro ideferword name,label,value
    iwordheader name
label:
    .word DODEFER
    .word value
.ident (.sprintf("PD%08X",PREVDEFER+1)):
    .word .ident (.sprintf("PD%08X",PREVDEFER))
    .word value
    PREVDEFER .set PREVDEFER + 1
    .endmacro

    .macro valueword name,label,value
	wordheader name
label:
    .word DOVALUE
	.word value
	.word DOVALUESTORE
	.word DOVALUEPSTORE
    .endmacro

