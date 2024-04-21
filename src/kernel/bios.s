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
    pha
    phx
    phy

    ; peak at the status register
    tsx

    lda $104,x
    and #BRK_MASK       ; are we here because of BRK?
    beq @notBRK         ; no

    lda MON_BP_ENABLE   ; are breakpoints enabled?
    beq @notBRK         ; no

    lda #$01            ; tell the monitor we're entering due to a breakpoint
    sta MON_FROM_BRK

    jmp KMONITOR

@notBRK:
    bit TERMIO::IREADY      ; was it a keyboard interrupt?
    bpl @irqDone            ; no

    jsr handleKeyboardIRQ   ; yes, go deal with it

@irqDone:
    ; restore state
    ply
    plx
    pla

    rti
