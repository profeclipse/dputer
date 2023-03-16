	.cpu "w65c02"

	.include "kernel.lbl"

	* = $B000

	lda #<LAB_getch
	sta VEC_IN
	lda #>LAB_getch
	sta VEC_IN+1
	lda #<LAB_putch
	sta VEC_OUT
	lda #>LAB_putch
	sta VEC_OUT+1
	lda #<no_load
	sta VEC_LD
	lda #>no_load
	sta VEC_LD+1
	lda #<no_save
	sta VEC_SV
	lda #>no_save
	sta VEC_SV+1

	JMP	LAB_COLD		; do EhBASIC cold start
	JMP	LAB_WARM		; do EhBASIC warm start

LAB_getch
	JSR KREADTERM
	CLC
	BEQ getchdone
	CMP #$7F
	BNE notdelete
	LDA #$08
notdelete
	SEC
getchdone
	RTS

LAB_putch
	JSR KWRITETERM
no_load
no_save
	RTS

LAB_mess
	.text	$0D,$0A,"6502 EhBASIC [C]old/[W]arm ?",$00
					; sign on string
REQ_text
	.text	"Derived from EhBASIC",$00

	.include "basic.asm"
