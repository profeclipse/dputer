    .segment "EXTRA"

CHRIN:
    jsr TERMGETCH
    jsr TERMWRITE
    sec
    rts

LCD_CMD:
    jsr GETBYT
    txa
    jmp LCDINST

LCD_CLEAR:
    jmp LCDCLEAR

LCD_PRINT:
    jsr FRMEVL
    bit VALTYP
    bmi @print
    jsr FOUT
    jsr STRLIT
@print:
    jsr FREFAC
    tax
    ldy #$00
@loop:
    lda (INDEX),y
    jsr LCDOUTCH
    iny
    dex
    bne @loop

    rts
