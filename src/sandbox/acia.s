    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "acia.inc"

    .segment "BIOS"

WDC_BUG = 1

acia_init:
    lda #ACIA_STATUS_RESET
    sta ACIA_STATUS
    lda #(ACIA_CTRL_1_STOP | ACIA_CTRL_8_BITS | ACIA_CTRL_RCS_BAUD | ACIA_CTRL_SBR_115K2)
    ;lda #(ACIA_CTRL_1_STOP | ACIA_CTRL_8_BITS | ACIA_CTRL_RCS_BAUD | ACIA_CTRL_SBR_19200)
    sta ACIA_CTRL
    lda #(ACIA_CMD_NP | ACIA_CMD_ECHO_OFF | ACIA_CMD_IRQ_LOFF | ACIA_CMD_RIRD_OFF | ACIA_CMD_DTRL)
    sta ACIA_CMD

    rts

acia_read:
    lda ACIA_STATUS
    and #ACIA_STATUS_RDRF
    beq acia_read

    lda ACIA_DATA

    rts

acia_write:
    pha
    sta ACIA_DATA
    .if WDC_BUG
    jsr acia_write_delay
    .else
acia_write_loop:
    lda ACIA_STATUS
    and #ACIA_STATUS_TDRE
    beq acia_write_loop
    .endif

    pla
    rts

    .if WDC_BUG
acia_write_delay:
    phx
    phy
    ldy #4
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
