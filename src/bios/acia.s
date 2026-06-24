    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "kernel.inc"
    .include "io.inc"
    .include "via.inc"
    .include "acia.inc"

    .segment "BIOS"

    .export acia_init, acia_read, acia_write

;WDC_BUG := 1

acia_init:
    lda #%00011111      ; 8-N-1, 19200 baud
    ;lda #%00010000      ; 8-N-1, 115.2k baud
    sta ACIA_CTRL
    lda #%10001001      ; No parity, no echo, rx interrupts
    sta ACIA_CMD

    lda #%00000001      ; hijacking PA0 for RTS flow control
    sta VIA0_DDRA
    lda #%11111110
    and VIA0_PORTA
    sta VIA0_PORTA

    rts

acia_read:
    lda ACIA_STATUS
    and #%00001000
    beq acia_read

    lda ACIA_DATA

    rts

acia_write:
    .ifdef WDC_BUG
    pha
    sta ACIA_DATA
    jsr acia_write_delay
    pla
    .else
    pha
acia_write_loop:
    lda ACIA_STATUS
    and #%00010000
    beq acia_write_loop
    pla
    sta ACIA_DATA
    .endif

    rts

    .ifdef WDC_BUG
acia_write_delay:
    phx
    phy
    ldy #MHZ
acia_write_loop_0:
    ldx #100
acia_write_loop_1:
    dex
    bne acia_write_loop_1
    dey
    bne acia_write_loop_0

    ply
    plx
    rts
    .endif
