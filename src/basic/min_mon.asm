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
	lda #<LAB_load
	sta VEC_LD
	lda #>LAB_load
	sta VEC_LD+1
	lda #<LAB_save
	sta VEC_SV
	lda #>LAB_save
	sta VEC_SV+1

    stz FILEIO_CFD
    stz tmp1

	jmp	LAB_COLD		; do EhBASIC cold start
	;JMP	LAB_WARM		; do EhBASIC warm start

LAB_getch
    lda FILEIO_CFD
    beq LAB_getch1
    lda FILEIO_CMODE
    cmp #FILEIO_MODE_READ
    bne LAB_getch1
    lda tmp1
    beq LAB_getch0
    dec tmp1
    lda #$0D
    bra notdelete
LAB_getch0
    jsr mon_ReadByte
    bra LAB_getch2
LAB_getch1
	jsr KREADTERM
LAB_getch2
	clc
	beq getchdone
	cmp #$7F
	bne notdelete
	lda #$08
notdelete
	sec
getchdone
	rts

LAB_putch
    pha
    lda FILEIO_CFD
    beq LAB_putch1
    lda FILEIO_CMODE
    cmp #FILEIO_MODE_WRITE
    beq LAB_putch0
    pla
    rts
LAB_putch0
    pla
    cmp #$0D
    bne LAB_putch0a 
    jsr mon_WriteByte
    lda #$0A
LAB_putch0a
    jsr mon_WriteByte
    bra LAB_putch2
LAB_putch1
    pla
	jsr KWRITETERM
LAB_putch2
	rts

LAB_load
    bne LAB_load0
    ldx #$24
    jmp LAB_XERR            ; missing file name
LAB_load0
    jsr mon_GetFileName
    bcc LAB_load1
    ldx #$24
    jmp LAB_XERR            ; bad filename
LAB_load1
    jsr mon_OpenFile
    bcc LAB_load2
    ldx #$26
    jmp LAB_XERR            ; open file error
LAB_load2
    tsx                     ; NEW clobbers return address, save it
    inx
    lda $100,x
    sta ptr1
    inx
    lda $100,x
    sta ptr1+1
    jsr LAB_1463            ; NEW
    lda ptr1+1              ; restore return address
    pha
    lda ptr1
    pha
    lda #1                  ; signal getch to return a newline
    sta tmp1
	rts

LAB_save
    bne LAB_save0
    ldx #$24
    jmp LAB_XERR            ; missing file name
LAB_save0
    jsr mon_GetFileName
    bcc LAB_save1
    ldx #$24
    jmp LAB_XERR            ; bad filename
LAB_save1
    jsr mon_CreateFile
    bcc LAB_save2
    ldx #$26
    jmp LAB_XERR            ; create file error
LAB_save2
    clc
    lda #0
    cmp #0
    jsr LAB_LIST            ; LIST
    jsr mon_CloseFile
    rts

mon_GetFileName
    jsr LAB_EVEX
    bit Dtypef
    bmi mon_GetFileName1       ; filename is good (maybe)
    bra mon_GetFileNameBad     ; filename not a string
mon_GetFileName1
    jsr LAB_22B6
    ldy #0
    tax
    beq mon_GetFileNameBad     ; filename is empty
mon_GetFileName2
    lda (ut1_pl),y
    sta FILEIO_FILENAME,y
    iny
    dex
    bne mon_GetFileName2
    lda #0
    sta FILEIO_FILENAME,y
    clc
    rts
mon_GetFileNameBad
    sec
    rts

mon_CreateFile
    lda #FILEIO_MODE_WRITE
    sta FILEIO_CMODE
    lda #FILEIO_CMD_OPEN
    sta FILEIO_CCMD
    jsr mon_doFileIO
    lda FILEIO_CSTATUS
    bne mon_CreateFileBad       ; open file failed - should do something informative here
    clc
    rts
mon_CreateFileBad
    sec
    rts

mon_OpenFile
    lda #FILEIO_MODE_READ
    sta FILEIO_CMODE
    lda #FILEIO_CMD_OPEN
    sta FILEIO_CCMD
    jsr mon_doFileIO
    lda FILEIO_CSTATUS
    bne mon_OpenFileBad         ; open file failed - should do something informative here
    clc
    rts
mon_OpenFileBad
    sec
    rts

mon_CloseFile
    lda #FILEIO_CMD_CLOSE
    sta FILEIO_CCMD
    jsr mon_doFileIO
    stz FILEIO_CFD
    clc
    rts

mon_WriteByte
    pha
    sta FILEIO_CDATA_LO
    lda #FILEIO_CMD_WRITE
    sta FILEIO_CCMD
    jsr mon_doFileIO
    lda FILEIO_CSTATUS
    clc
    beq mon_WRiteByteDone
    sec
mon_WriteByteDone
    pla
    rts

mon_ReadByte
    lda #FILEIO_CMD_READ
    sta FILEIO_CCMD
    jsr mon_doFileIO
    lda FILEIO_CSTATUS
    bne mon_ReadByteEOF
    lda FILEIO_CDATA_LO
    bra mon_ReadByteDone
mon_ReadByteEOF
    jsr mon_CloseFile
    lda #0
mon_ReadByteDone
    rts

mon_waitFileIO
    lda FILEIO_CREADY
    bne mon_waitFileIO
    rts

mon_doFileIO
    lda #1
    sta FILEIO_CREADY
    bra mon_waitFileIO

LAB_mess
	.text	$0D,$0A,"6502 EhBASIC [C]old/[W]arm ?",$00
					; sign on string
REQ_text
	.text	"Derived from EhBASIC",$00

	.include "basic.asm"
