    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "bios.inc"

    .segment "BIOS"

bios:
    ldx #$ff
    txs

    jsr lcd_init
    jsr acia_init
    jsr term_init
    jsr timer_init

    jsr show_welcome

    cli

    jmp monitor

show_welcome:
    ldx #0
@L0:
    lda welcome,x
    beq @L1
    jsr lcd_outch
    jsr acia_write
    inx
    bra @L0

@L1:
    lda #$0D
    jsr acia_write
    lda #$0A
    jsr acia_write
    rts

biosIRQ:
    pha
    phx
    phy

    jsr timerIRQ
    jsr termIRQ

    ply
    plx
    pla

    rti

welcome:
    .asciiz "DPuter v1.0"

    .include "zeropage.s"
    .include "lcd.s"
    .include "acia.s"
    .include "term.s"
    .include "timer.s"
    .include "monitor.s"
    .include "jumptable.s"
    .include "vectors.s"
