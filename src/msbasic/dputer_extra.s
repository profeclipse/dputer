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
    jsr GETBYT
    txa
    jmp LCDOUTCH
