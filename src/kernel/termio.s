    .setcpu "65C02"

    .segment "KERNEL"

; ============================================================================
; Terminal Initialization
; ============================================================================

; ****************************************************************************
; Function:     KTERMINIT
; Description:  Initializes terminal I/O
; Input:        nothing
; Changes:      a
; Returns:      nothing
; ****************************************************************************
KTERMINIT:
    stz TERMIO::IREADY
    stz TERMIO::IDATA
    stz TERMIO::OREADY
    stz TERMIO::ODATA
    stz TERMIO::CREADY
    stz TERMIO::CCMD
    stz TERMIO::CDATA
    stz KBD_WPTR
    stz KBD_RPTR

    lda #<INPUT_BUFFER
    sta STRIN_VECTOR
    lda #>INPUT_BUFFER
    sta STRIN_VECTOR+1

    rts

; ============================================================================
; Terminal Commands
; ============================================================================

; ----------------------------------------------------------------------------
; Wait for CREADY to clear
; ----------------------------------------------------------------------------
kWaitForTerminalCmd:
@checkReady:
    bit TERMIO::CREADY
    bmi @checkReady

    rts

; ****************************************************************************
; Function:     KCLS
; Description:  Clear screen
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KCLS:
    pha

    lda #12
    jsr KWRITETERM

    pla

    rts


; ****************************************************************************
; Function:     KHOME
; Description:  Move the cursor to the home position (0,0)
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KHOME:
    pha

    jsr kWaitForTerminalCmd

    lda #TERMIO_HOME
    sta TERMIO::CCMD
    stz TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    pla
    rts



; ****************************************************************************
; Function:     KSETCURX
; Description:  Set cursor x position
; Input:        x - desired x position
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KSETCURX:
    pha

    jsr kWaitForTerminalCmd

    lda #TERMIO_CURSOR_X
    sta TERMIO::CCMD
    stx TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    pla
    rts


; ****************************************************************************
; Function:     KSETCURY
; Description:  Set cursor y position
; Input:        y - desired y position
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KSETCURY:
    pha

    jsr kWaitForTerminalCmd

    lda #TERMIO_CURSOR_Y
    sta TERMIO::CCMD
    sty TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    pla
    rts


; ****************************************************************************
; Function:     KSETCURXY
; Description:  Set cursor x,y position
; Input:        x - desired x position
;               y - desired y position
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KSETCURXY:
    jsr KSETCURX
    bra KSETCURY


; ****************************************************************************
; Function:     KGETSCRW
; Description:  Get screen width
; Input:        nothing
; Changes:      a
; Returns:      a - width of screen
; ****************************************************************************
KGETSCRW:
    jsr kWaitForTerminalCmd

    lda #TERMIO_SCREEN_W
    sta TERMIO::CCMD
    stz TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    jsr kWaitForTerminalCmd

    lda TERMIO::CDATA

    rts



; ****************************************************************************
; Function:     KGETSCRH
; Description:  Get screen height
; Input:        nothing
; Changes:      a
; Returns:      a - height of screen
; ****************************************************************************
KGETSCRH:
    jsr kWaitForTerminalCmd

    lda #TERMIO_SCREEN_H
    sta TERMIO::CCMD
    stz TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    jsr kWaitForTerminalCmd

    lda TERMIO::CDATA

    rts


; ****************************************************************************
; Function:     KGETCURX
; Description:  Get cursor x position
; Input:        nothing
; Changes:      a
; Returns:      a - cursor x position
; ****************************************************************************
KGETCURX:
    jsr kWaitForTerminalCmd
    
    lda #TERMIO_GETCUR_X
    sta TERMIO::CCMD
    stz TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    jsr kWaitForTerminalCmd

    lda TERMIO::CDATA

    rts


; ****************************************************************************
; Function:     KGETCURY
; Description:  Get cursor y position
; Input:        nothing
; Changes:      a
; Returns:      a - cursor y position
; ****************************************************************************
KGETCURY:
    jsr kWaitForTerminalCmd
    
    lda #TERMIO_GETCUR_Y
    sta TERMIO::CCMD
    stz TERMIO::CDATA
    lda #$ff
    sta TERMIO::CREADY

    jsr kWaitForTerminalCmd

    lda TERMIO::CDATA

    rts


; ============================================================================
; Terminal Output
; ============================================================================

; ----------------------------------------------------------------------------
; Wait for OREADY to clear
; ----------------------------------------------------------------------------
kWaitForTerminalOut:
@checkReady:
    bit TERMIO::OREADY
    bmi @checkReady
    
    rts

; ****************************************************************************
; Function:     KWRITETERM
; Description:  Write a character to the terminal
; Input:        a - character to output
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KWRITETERM:
    pha
    pha
    
    jsr kWaitForTerminalOut

    pla
    sta TERMIO::ODATA
    lda #$ff
    sta TERMIO::OREADY

    pla

    rts

; ****************************************************************************
; Function:     KWRITESTR
; Description:  Writes a z-string to the terminal
; Input:        x,y - address of string (x-lo,y-hi)
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KWRITESTR:
    pha

    stx STROUT_VECTOR
    sty STROUT_VECTOR+1

@writeChar:
    lda (STROUT_VECTOR)
    beq @done

    jsr KWRITETERM

    inc STROUT_VECTOR
    bne @writeChar
    inc STROUT_VECTOR+1
    bra @writeChar

@done:
    pla

    rts

; ****************************************************************************
; Function:     KWRITECRLF
; Description:  Write carriage-return/line-feed to terminal
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KWRITECRLF:
    pha

    lda #$0D
    jsr KWRITETERM

    pla
    rts


; ============================================================================
; Terminal Input
; ============================================================================

; ****************************************************************************
; Function:     KREADTERM
; Description:  Read a character from the terminal
; Input:        nothing
; Changes:      a
; Returns:      character in a, 0 if no character
; Flags:        Carry set if character, clear otherwise
; ****************************************************************************

KREADTERM:
    sei

    lda KBD_RPTR
    cmp KBD_WPTR
    bne @haveCharacter

    clc
    bra @done

@haveCharacter:
    phx

    ; keyboard read pointer is in a
    tax
    lda KBD_BUFFER,x
    inc KBD_RPTR

    plx

    sec

@done:
    cli
    rts


; ****************************************************************************
; Function:     KUNREADTERM
; Description:  Unread a character from the terminal
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:
; ****************************************************************************

KUNREADTERM:
    sei
    dec KBD_RPTR
    cli
    rts


; ****************************************************************************
; Function:     KCHECKTERM
; Description:  Check if terminal character available
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:        Carry set if character, clear otherwise
; ****************************************************************************

KCHECKTERM:
    lda KBD_RPTR
    cmp KBD_WPTR
    beq @noCharacter

    sec
    bra @done

@noCharacter:
    clc

@done:
    rts


; ****************************************************************************
; Function:     KGETCHAR
; Description:  Get terminal character, wait if none
; Input:        nothing
; Changes:      a
; Returns:      character in a
; Flags:        Carry set if character, clear otherwise
; ****************************************************************************
KGETCHAR:
    jsr KREADTERM
    bcc KGETCHAR

    rts


; ****************************************************************************
; Function:     KGETSTR
; Description:  Get string from terminal, wait for CR
; Input:        nothing
; Changes:      a
; Returns:      asciiz string stored at INPUT_BUFFER
;               a = length of string
; Flags:        Zero flag set if length of string is zero
; ****************************************************************************
KGETSTR:
    phy

    ldy #$00
@nextChar:
    jsr KGETCHAR

    cmp #$08                    ; is it backspace?
    beq @doBackspace

    cmp #$7F                    ; is it delete?
    bne @checkCRLF

@doBackspace:
    cpy #$00
    beq @nextChar

    dey
    jsr KWRITETERM              ; echo backspace

    bra @nextChar

@checkCRLF:
    cmp #$0D                    ; is it a carriage return?
    beq @done

    cmp #$0A                    ; is it a newline?
    beq @done

    cpy #INPUT_BUFFER_LEN       ; is the buffer full?
    bne @storeChar

    lda #$07                    ; yes, sound the alarm
    jsr KWRITETERM
    bra @nextChar

@storeChar:
    sta (STRIN_VECTOR),y        ; add character to input buffer
    jsr KWRITETERM              ; echo character

    iny
    bra @nextChar

@done:
    lda #$00                    ; null terminate the string
    sta (STRIN_VECTOR),y

    lda #$0D                    ; echo CRLF
    jsr KWRITETERM

    tya                         ; return length in a

    ply

    and #$FF                    ; reset zero flag for length

    rts

; ============================================================================
; Terminal Input Interrupt Handler
; ============================================================================

handleKeyboardIRQ:
    lda TERMIO::IDATA
    ldx KBD_WPTR
    sta KBD_BUFFER,x
    inc KBD_WPTR
    stz TERMIO::IREADY
    rts
