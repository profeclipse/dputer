    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "lcd.inc"

    .segment "BIOS"

lcd_outch:
    jsr lcd_wait
    pha

    pha
    lsr
    lsr
    lsr
    lsr
    ora #LCD_RS
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    eor #LCD_E
    sta LCD_PORT
    pla
    and #%00001111
    ora #LCD_RS
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    eor #LCD_E
    sta LCD_PORT

    pla

    rts

lcd_print:
    pha
    phy

    stx STROUT_VECTOR
    sty STROUT_VECTOR+1

    ldy #$00
@writeChar:
    lda (STROUT_VECTOR),y
    beq @done

    jsr lcd_outch

    iny
    bra @writeChar

@done:
    ply
    pla

    rts

lcd_init:
    lda #%11111111              ; set all pins on LCD_DATA port to output
    sta LCD_DDR

    lda #%00000011              ; set 8-bit mode
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT

    lda #%00000011              ; set 8-bit mode
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT

    lda #%00000011              ; set 8-bit mode
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT

    lda #%00000010              ; set 4-bit mode
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT

    lda #%00101000              ; set 4-bit mode, 2-line display, 5x8 font
    jsr lcd_instruction
    lda #%00001110              ; display on, cursor on, blink off
    jsr lcd_instruction
    lda #%00000110              ; increment cursor, don't shift display
    jsr lcd_instruction
    lda #%00000001              ; clear display
    jsr lcd_instruction

    rts

lcd_clear:
    pha
    lda #%00000001
    jsr lcd_instruction
    pla

    rts

lcd_instruction:
    jsr lcd_wait
    pha

    lsr
    lsr
    lsr
    lsr
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    eor #LCD_E
    sta LCD_PORT

    pla
    and #%00001111
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    eor #LCD_E
    sta LCD_PORT

    rts

lcd_wait:
    pha

    lda #%11110000              ; Set LCD_DATA port lcd bits to input
    sta LCD_DDR
lcd_busy:
    lda #LCD_RW
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    lda LCD_PORT
    pha
    lda #LCD_RW
    sta LCD_PORT
    ora #LCD_E
    sta LCD_PORT
    lda LCD_PORT
    pla
    and #LCD_BUSY
    bne lcd_busy

    lda #LCD_RW 
    sta LCD_PORT
    lda #%11111111              ; Reset LCD_DATA port to output
    sta LCD_DDR

    pla
    rts
