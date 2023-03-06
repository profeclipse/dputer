;*****************************************************************************
; kernel.asm
;*****************************************************************************
; Memory Map:
; 0000-002F: general purpose vectors used by kernel and monitor
; 0030-00FF: free zero-page
; 0100-01FF: stack
; 0200-DBFF: free memory
; DC00-DDFF: general kernel data
;	DC00-DC0F: terminal i/o
;	DC10-DCFF: file i/o (this needs to change)
;	DD00-DDFF: keyboard ring buffer
;	DE00-DEFF: input buffer
; E000-FFFF: kernel code
;-----------------------------------------------------------------------------

		.cpu "w65c02"

		.include "macros.inc"
		.include "termio.inc"
		.include "kernel.inc"
		.include "fileio.inc"

        * = $E000

KERNELSTART = *
;*****************************************************************************
;				KRESET
; Description:	RESET signal handler
;				Initialize system and enter monitor.
;*****************************************************************************
KRESET .block

kRESET1
		cld

		ldx #$ff
		txs

        jsr KTERMINIT
		jsr KFILEINIT
		jsr KMONINIT

        cli

		jsr KCLS

		ldx #<kwelcome
		ldy #>kwelcome
		jsr KWRITESTR

		jmp MONITOR

kwelcome
		.null "*** DPuter W65C02S v1.0 by David Hunt - 54K Bytes Free",$0a

KFILEINIT:
		stz FILEIO_CREADY
		stz FILEIO_CCMD
		stz FILEIO_CFD
		stz FILEIO_CMODE
		stz FILEIO_CDATA_LO
		stz FILEIO_CDATA_HI
		stz FILEIO_CDATA_LO2
		stz FILEIO_CDATA_HI2
		stz FILEIO_CSTATUS
		stz FILEIO_FILENAME
		rts

		.endblock

		.include "termio.asm"


;*****************************************************************************
; Function:		IRQ handler.
; Description:	This is the global IRQ handler. It distinguishes between a
;				regular IRQ and a BRK instruction. If BRK caused IRQ, then
;				the monitor is called.
;*****************************************************************************
DOIRQ .block
		sta IRQ_SAVE_A			; save registers
		stx IRQ_SAVE_X
		sty IRQ_SAVE_Y
		tsx
		stx IRQ_SAVE_SP

		pla						; peek at the status register
		pha

		and #$10				; are we here because of BRK?
		beq notBRK				; no, forget we were here

		lda MON_BP_ENABLE		; are breakpoints enabled?
		beq notBRK				; no, again forget we were here

		lda #$1					; indicate we're entering monitor from BRK
		sta MON_FROM_BRK

		pla						; yes, pop the status and return address
		sta BRK_STATUS_REG
		plx
		stx BRK_PC_VECTOR
		plx
		stx BRK_PC_VECTOR+1

		pha

		lda BRK_PC_VECTOR		; backup PC vector to actual BRK instruction
		sec
		sbc #$02
		sta BRK_PC_VECTOR
		lda BRK_PC_VECTOR+1
		sbc #$00
		sta BRK_PC_VECTOR+1

		ldx IRQ_SAVE_SP
		stx BRK_SAVE_SP
		txs
		ldy IRQ_SAVE_Y			; restore registers
		sty BRK_SAVE_Y
		ldx IRQ_SAVE_X
		stx BRK_SAVE_X
		lda IRQ_SAVE_A
		sta BRK_SAVE_A

		plp

		jmp MONITOR

notBRK
		; not BRK. was it keyboard input?
        lda TERMIO_IREADY
		beq notKBD

		jsr kHandleKBD

notKBD
		ldx IRQ_SAVE_SP
		txs
		ldy IRQ_SAVE_Y	; restore registers and return
		ldx IRQ_SAVE_X
		lda IRQ_SAVE_A

	    rti

;*****************************************************************************
; Function:		kHandleKBD
; Description:	Keyboard interrupt handler
; Input:		Nothing
; Changes:		a,x
; Returns:		Nothing
;*****************************************************************************
kHandleKBD .block
		lda TERMIO_IDATA
		ldx KBD_WPTR
		sta KBD_BUFFER,x
		inc KBD_WPTR
		stz TERMIO_IREADY

		rts
		.endblock
		.endblock


;*****************************************************************************
; NMI handler. Just do nothing.
;*****************************************************************************
DONMI
		rti

		.include "monitor.asm"


;*****************************************************************************
; Kernel Jump Table
;*****************************************************************************
		* = $FF80
CHROUT		jmp KWRITETERM
CHRIN		jmp KGETCHAR
CHRCHK		jmp KCHECKTERM
KMON		jmp MONITOR
KWHEXBYT	jmp MONITOR.MWriteHexByte
KPUTSTR		jmp MONITOR.MPutStr


        * = $FFFA
NMIVEC	.word DONMI
RESVEC	.word KRESET
IRQVEC	.word DOIRQ
