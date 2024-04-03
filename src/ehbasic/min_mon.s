    .setcpu "65C02"
    .feature org_per_seg
    .debuginfo

    .include "kernel.inc"
	.include "kernel.lbl"

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

    stz FILEIO::FD
    stz tmp1

	jmp	LAB_COLD		; do EhBASIC cold start

LAB_getch:
    lda FILEIO::FD
    beq LAB_getch1
    lda FILEIO::MODE
    eor #FILEIO_MODE_BIN
    cmp #FILEIO_MODE_READ
    bne LAB_getch1
    lda tmp1
    beq LAB_getch0
    dec tmp1
    lda #$0D
    bra notdelete
LAB_getch0:
    jsr mon_ReadByte
    bra LAB_getch2
LAB_getch1:
	jsr KREADTERM
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
    pha
    lda FILEIO::FD
    beq LAB_putch1
    lda FILEIO::MODE
    eor #FILEIO_MODE_BIN
    cmp #FILEIO_MODE_WRITE
    beq LAB_putch0
    pla
    rts
LAB_putch0:
    pla
    cmp #$0D
    bne LAB_putch0a 
    jsr mon_WriteByte
    lda #$0A
LAB_putch0a:
    jsr mon_WriteByte
    bra LAB_putch2
LAB_putch1:
    pla
	jsr KWRITETERM
LAB_putch2:
	rts

LAB_load:
    bne LAB_load0
    ldx #$24
    jmp LAB_XERR            ; missing file name
LAB_load0:
    jsr mon_GetFileName
    bcc LAB_load1
    ldx #$24
    jmp LAB_XERR            ; bad filename
LAB_load1:
    jsr mon_OpenFile
    bcc LAB_load2
    ldx #$26
    jmp LAB_XERR            ; open file error
LAB_load2:
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

LAB_save:
    bne LAB_save0
    ldx #$24
    jmp LAB_XERR            ; missing file name
LAB_save0:
    jsr mon_GetFileName
    bcc LAB_save1
    ldx #$24
    jmp LAB_XERR            ; bad filename
LAB_save1:
    jsr mon_CreateFile
    bcc LAB_save2
    ldx #$26
    jmp LAB_XERR            ; create file error
LAB_save2:
    clc
    lda #0
    cmp #0
    jsr LAB_LIST            ; LIST
    jsr mon_CloseFile
    rts

mon_GetFileName:
    jsr LAB_EVEX
    bit Dtypef
    bmi mon_GetFileName1       ; filename is good (maybe)
    bra mon_GetFileNameBad     ; filename not a string
mon_GetFileName1:
    jsr LAB_22B6
    ldy #0
    tax
    beq mon_GetFileNameBad     ; filename is empty
mon_GetFileName2:
    lda (ut1_pl),y
    sta FILEIO::FILENAME,y
    iny
    dex
    bne mon_GetFileName2
    lda #0
    sta FILEIO::FILENAME,y
    clc
    rts
mon_GetFileNameBad:
    sec
    rts

mon_CreateFile:
    lda #FILEIO_MODE_WRITE
    ora #FILEIO_MODE_BIN
    sta FILEIO::MODE
    lda #FILEIO_OPEN
    sta FILEIO::CMD
    jsr mon_doFileIO
    lda FILEIO::STATUS
    bne mon_CreateFileBad       ; open file failed - should do something informative here
    clc
    rts
mon_CreateFileBad:
    sec
    rts

mon_OpenFile:
    lda #FILEIO_MODE_READ
    ora #FILEIO_MODE_BIN
    sta FILEIO::MODE
    lda #FILEIO_OPEN
    sta FILEIO::CMD
    jsr mon_doFileIO
    lda FILEIO::STATUS
    bne mon_OpenFileBad         ; open file failed - should do something informative here
    clc
    rts
mon_OpenFileBad:
    sec
    rts

mon_CloseFile:
    lda #FILEIO_CLOSE
    sta FILEIO::CMD
    jsr mon_doFileIO
    stz FILEIO::FD
    clc
    rts

mon_WriteByte:
    pha
    sta FILEIO::DATA_LO
    lda #FILEIO_WRITE
    sta FILEIO::CMD
    jsr mon_doFileIO
    lda FILEIO::STATUS
    clc
    beq mon_WriteByteDone
    sec
mon_WriteByteDone:
    pla
    rts

mon_ReadByte:
    lda #FILEIO_READ
    sta FILEIO::CMD
    jsr mon_doFileIO
    lda FILEIO::STATUS
    bne mon_ReadByteEOF
    lda FILEIO::DATA_LO
    bra mon_ReadByteDone
mon_ReadByteEOF:
    jsr mon_CloseFile
    lda #0
mon_ReadByteDone:
    rts

mon_waitFileIO:
    bit FILEIO::READY
    bmi mon_waitFileIO
    rts

mon_doFileIO:
    lda #$FF
    sta FILEIO::READY
    bra mon_waitFileIO

LAB_mess:
	.byte	$0D,$0A,"6502 EhBASIC [C]old/[W]arm ?",$00
					; sign on string
REQ_text:
	.byte	"Derived from EhBASIC",$00

    .include "basic.s"
