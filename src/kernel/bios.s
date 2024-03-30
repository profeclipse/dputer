    .setcpu "65C02"

    .segment "KERNEL"

KERNELSTART = *

KRESET:
    cld

    ldx #$ff
    txs

    jsr KTERMINIT
    jsr KFILEINIT
    jsr KMONINIT

    cli

    jsr KCLS

    ldx #<welcome
    ldy #>welcome
    jsr KWRITESTR

    jmp KMONITOR

welcome:
	.asciiz "*** DPuter W65C02S v1.0 by David Hunt - 54K Bytes Free\n"

NMI_HANDLER:
    rti


;*****************************************************************************
; Function:     IRQ handler.
; Description:  This is the global IRQ handler. It distinguishes between a
;               regular IRQ and a BRK instruction. If BRK caused IRQ, then
;               the monitor is called.
;*****************************************************************************
IRQ_HANDLER:
    ; save registers
    sta IRQ_SAVE_A
    stx IRQ_SAVE_X
    sty IRQ_SAVE_Y
    tsx
    stx IRQ_SAVE_SP

    ; peak at the status register
    pla
    pha

    and #BRK_MASK       ; are we here because of BRK?
    beq @notBRK         ; no

    lda MON_BP_ENABLE   ; are breakpoints enabled?
    beq @notBRK         ; no

    lda #$01            ; tell the monitor we're entering due to a breakpoint
    sta MON_FROM_BRK

    ; pop and save the status register and return IRQ return addres
    pla
    sta BRK_STATUS_REG
    plx
    stx BRK_PC_VECTOR
    plx
    stx BRK_PC_VECTOR+1

    pha                 ; repush status register

    ; backup PC vector to actual BRK instruction
    sec
    lda BRK_PC_VECTOR
    sbc #$02
    sta BRK_PC_VECTOR
    lda BRK_PC_VECTOR+1
    sbc #$00
    sta BRK_PC_VECTOR+1

    ; restore state
    ldx IRQ_SAVE_SP
    stx BRK_SAVE_SP
    txs
    ldy IRQ_SAVE_Y
    sty BRK_SAVE_Y
    ldx IRQ_SAVE_X
    stx BRK_SAVE_X
    lda IRQ_SAVE_A
    sta BRK_SAVE_A

    plp

    jmp KMONITOR

@notBRK:
    lda TERMIO::IREADY       ; was it a keyboard interrupt?
    beq @irqDone             ; no

    jsr handleKeyboardIRQ   ; yes, go deal with it

@irqDone:
    ; restore state
    ldx IRQ_SAVE_SP
    txs
    ldy IRQ_SAVE_Y
    ldx IRQ_SAVE_X
    lda IRQ_SAVE_A

    rti

