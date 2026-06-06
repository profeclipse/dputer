    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "io.inc"
    .include "via.inc"

    .segment "BIOS"

    .importzp TIMER_TICKS, SLEEP_TIME, SLEEP_TICKS
    .export timer_init, timer_sleep, timerIRQ

TENMILLIS = 40000 - 2

; ****************************************************************************
; Function:     timer_init
; Description:  Initialize the timer to generate an interrupt every 10ms.
; Input:        nothing
; Output:       nothing
; Changes:      A
; ****************************************************************************
timer_init:
    stz TIMER_TICKS
    stz TIMER_TICKS+1
    stz TIMER_TICKS+2
    stz TIMER_TICKS+3

    lda #%01000000    ; Enable timer 1 interrupt
    sta VIA0_ACR

    ; Initiaize timer to generate an interrupt every 10ms. The timer counts
    ; down from the value we set, so we set it to 40000 (40k) - 2, which is
    ; about 10ms at 4MHz. We subtract 2 because the timer takes 2 cycles to
    ; load the value, so the first interrupt will happen after 39998 cycles.
    lda #<TENMILLIS
    sta VIA0_T1CL
    lda #>TENMILLIS
    sta VIA0_T1CH

    lda #%01111111    ; Disable all VIA interrupts
    lda #%11000000    ; Enable VIA T1 interrupts
    sta VIA0_IER

    rts

; ****************************************************************************
; Function:     timer_sleep
; Description:  Sleep for a specified number of timer ticks (10ms each).
; Input:        Number of ticks to sleep in A
; Output:       nothing
; Changes:      nothing
; ****************************************************************************
timer_sleep:
    pha
    sta SLEEP_TIME
    lda TIMER_TICKS
    sta SLEEP_TICKS

@wait:
    lda TIMER_TICKS
    sec
    sbc SLEEP_TICKS
    cmp SLEEP_TIME
    bcc @wait
    pla
    rts

; ****************************************************************************
; Function:     timerIRQ
; Description:  Interrupt handler for the timer. Increments the timer tick
;               count.
; Input:        nothing
; Output:       nothing
; ****************************************************************************
timerIRQ:
    bit VIA0_IFR
    bpl @noIRQ          ; no VIA interrupt
    bvc @noIRQ          ; not a timer interrupt

    inc TIMER_TICKS
    bne @done
    inc TIMER_TICKS+1
    bne @done
    inc TIMER_TICKS+2
    bne @done
    inc TIMER_TICKS+3

@done:
    bit VIA0_T1CL

@noIRQ:
    rts
