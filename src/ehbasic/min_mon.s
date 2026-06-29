    .setcpu "W65C02"
    .feature org_per_seg
    .debuginfo

    .include "bios.inc"

    .include "zeropage.s"

    .segment "LOADADDR"

    .word BASICSTART

    .segment "BASIC"

BASICSTART = *

	lda #<LAB_getch
	sta VEC_IN
	lda #>LAB_getch
	sta VEC_IN+1
	lda #<LAB_putch
	sta VEC_OUT
	lda #>LAB_putch
	sta VEC_OUT+1
	lda #<LAB_load
	sta VEC_LD
	lda #>LAB_load
	sta VEC_LD+1
	lda #<LAB_save
	sta VEC_SV
	lda #>LAB_save
	sta VEC_SV+1

	jmp	LAB_COLD		; do EhBASIC cold start

LAB_getch:
	jsr TERMREAD
LAB_getch2:
    bcc getchdone
	cmp #$7F
	bne notdelete
	lda #$08
notdelete:
	sec
getchdone:
	rts

LAB_putch:
	jsr TERMWRITE
	rts

LAB_load:
	rts

LAB_save:
    rts

LAB_mess:
	.byte	$0D,$0A,"6502 EhBASIC [C]old/[W]arm ?",$00
					; sign on string
REQ_text:
	.byte	"Derived from EhBASIC",$0D,$0A,$00

    .include "basic.s"
