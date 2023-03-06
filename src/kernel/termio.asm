;*****************************************************************************
; termio.asm
;*****************************************************************************

KREADTERM .block
				sei
				lda KBD_RPTR
				cmp KBD_WPTR
				bne +

				lda #$0
				bra ++

+
				phx
				tax
				lda KBD_BUFFER,x
				inc KBD_RPTR
				plx
				cmp #$0

+
				cli
				rts
				.endblock

KUNREADTERM .block
				sei
				dec KBD_RPTR
				cli
				rts
				.endblock

KCHECKTERM .block
				;sei
				lda KBD_RPTR
				cmp KBD_WPTR
				bne +

				lda #0
				bra ++
+
				lda #$ff
+
				;cli
				rts
				.endblock

;*****************************************************************************
; Function:		KGETCHAR
; Description:	Read a character from the terminal. Waits until key received.
; Input:		nothing
; Changes:		a
; Returns:		a - character
;*****************************************************************************
KGETCHAR .block
				jsr KREADTERM
				beq KGETCHAR

				rts
				.endblock


;*****************************************************************************
; Function:		KGETSTR
; Description:	Read a string from the terminal
; Input:		nothing
; Changes:		a
; Returns:		z-string stored at INPUT_BUFFER
;				a - length of string
;*****************************************************************************
KGETSTR .block
				phy

				ldy #$00
nextChar
				jsr KGETCHAR
				cmp #$08
				beq doBackspace

				cmp #$7F
				bne notBackspace

doBackspace
				cpy #$00
				beq nextChar

				dey
				jsr KWRITETERM
				bra nextChar

notBackspace
				cmp #$0d
				beq done
				cmp #$0a
				beq done

				cpy #INPUT_BUFFER_LEN
				bne storeChar

				lda #$07
				jsr KWRITETERM
				bra nextChar

storeChar
				sta (STRIN_VECTOR),y
				jsr KWRITETERM

				inc	y
				bra nextChar

done
				lda #$00
				sta (STRIN_VECTOR),y

				lda #$0d
				jsr KWRITETERM

				tya

				ply

				and #$ff

				rts
				.endblock


;*****************************************************************************
; Function:		KWRITETERM
; Description:	Write a character to the terminal
; Input:		a - character to output
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KWRITETERM .block
				pha
				pha

readyStatus
				lda TERMIO_OREADY
				bne readyStatus

				pla
				sta TERMIO_ODATA

				lda #$ff
				sta TERMIO_OREADY

				pla

				rts
				.endblock


;*****************************************************************************
; Function:		KWRITESTR
; Description:	Writes a z-string to the terminal
; Input:		x,y - address of string (x-lo,y-hi)
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KWRITESTR .block
				pha
				phy

				stx STROUT_VECTOR
				sty STROUT_VECTOR+1

writeChar
				lda (STROUT_VECTOR)
				beq done

				jsr KWRITETERM

				clc
				lda STROUT_VECTOR
				adc #1
				sta STROUT_VECTOR
				lda STROUT_VECTOR+1
				adc #0
				sta STROUT_VECTOR+1
				bra writeChar

done
				ply
				pla

				rts
				.endblock


;*****************************************************************************
; Function:		KWRITECRLF
; Description:	Write carriage-return/line-feed to terminal
; Input:		nothing
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KWRITECRLF .block
				pha

				lda #$0a
				jsr KWRITETERM

				pla
				rts
				.endblock


;*****************************************************************************
; Function:		KCLS
; Description:	Clear screen
; Input:		nothing
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KCLS .block
				pha
-
				lda TERMIO_CREADY
				bne -

				lda #TERMIO_CMD_CLS
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

				pla
				rts
				.endblock


;*****************************************************************************
; Function:		KHOME
; Description:	Move cursor to home position (0,0)
; Input:		nothing
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KHOME .block
				pha
-
				lda TERMIO_CREADY
				bne -

				lda #TERMIO_CMD_HOME
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

				pla
				rts
				.endblock


;*****************************************************************************
; Function:		KSETCURX
; Description:	Set cursor x position
; Input:		x - x position
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KSETCURX .block
				pha
-
				lda TERMIO_CREADY
				bne -

				lda #TERMIO_CMD_CURSOR_X
				sta TERMIO_CCMD
				stx TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

				pla
				rts
				.endblock


;*****************************************************************************
; Function:		KSETCURY
; Description:	Set cursor y position
; Input:		y - y position
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KSETCURY .block
				pha
-
				lda TERMIO_CREADY
				bne -

				lda #TERMIO_CMD_CURSOR_Y
				sta TERMIO_CCMD
				sty TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

				pla
				rts
				.endblock


;*****************************************************************************
; Function:		KSETCURXY
; Description:	Set cursor x,y position
; Input:		x - x position
;				y - y position
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
KSETCURXY .block
				jsr KSETCURX
				bra KSETCURY
				.endblock

;*****************************************************************************
; Function:		KGETSCRW
; Function:		Get screen width
; Input:		nothing
; Changes:		A
; Returns:		width in A
;*****************************************************************************
KGETSCRW .block
				lda TERMIO_CREADY
				bne KGETSCRW

				lda #TERMIO_CMD_SCREEN_W
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

-
				lda TERMIO_CREADY
				bne -

				lda TERMIO_CDATA
				rts
				.endblock


;*****************************************************************************
; Function:		KGETSCRH
; Function:		Get screen height
; Input:		nothing
; Changes:		A
; Returns:		height in A
;*****************************************************************************
KGETSCRH .block
				lda TERMIO_CREADY
				bne KGETSCRH

				lda #TERMIO_CMD_SCREEN_H
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

-
				lda TERMIO_CREADY
				bne -

				lda TERMIO_CDATA
				rts
				.endblock


;*****************************************************************************
; Function:		KGETCURX
; Function:		Get current cursor X position
; Input:		nothing
; Changes:		A
; Returns:		x-position in A
;*****************************************************************************
KGETCURX .block
				lda TERMIO_CREADY
				bne KGETCURX

				lda #TERMIO_CMD_GETCUR_X
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

termWait
				lda TERMIO_CREADY
				bne termWait

				lda TERMIO_CDATA
				rts
				.endblock


;*****************************************************************************
; Function:		KGETCURY
; Function:		Get current cursor Y position
; Input:		nothing
; Changes:		A
; Returns:		y-position in A
;*****************************************************************************
KGETCURY .block
				lda TERMIO_CREADY
				bne KGETCURY

				lda #TERMIO_CMD_GETCUR_Y
				sta TERMIO_CCMD
				stz TERMIO_CDATA
				lda #$ff
				sta TERMIO_CREADY

termWait
				lda TERMIO_CREADY
				bne termWait

				lda TERMIO_CDATA
				rts
				.endblock


;*****************************************************************************
; Function:		KTERMINIT
; Description:	Initializes terminal I/O
; Input:		nothing
; Changes:		a
; Returns:		nothing
;*****************************************************************************
KTERMINIT .block
				stz TERMIO_IREADY
				stz TERMIO_IDATA
				stz TERMIO_OREADY
				stz TERMIO_ODATA
				stz TERMIO_CREADY
				stz TERMIO_CCMD
				stz TERMIO_CDATA
				stz KBD_WPTR
				stz KBD_RPTR

				lda #<INPUT_BUFFER
				sta STRIN_VECTOR
				lda #>INPUT_BUFFER
				sta STRIN_VECTOR+1

				rts
				.endblock
