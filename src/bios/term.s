    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "io.inc"
    .include "acia.inc"
    .include "term.inc"

    .segment "BIOS"

    .importzp KBD_WPTR, KBD_RPTR, STRIN_VECTOR, STROUT_VECTOR
    .import acia_write
    .export term_init, term_write_buffer, term_read_buffer, term_read
    .export term_get_char, term_get_string, term_unread, term_haschar
    .export term_write, term_write_string, term_write_crlf, term_get_buffer_space
    .export termIRQ

; ****************************************************************************
; Function:     term_init
; Description:  Initialize the terminal
; Input:        nothing
; Changes:      A
; Returns:      nothing
; Flags:        z
; ****************************************************************************
term_init:
    stz KBD_WPTR
    stz KBD_RPTR
    lda #<INPUT_BUFFER
    sta STRIN_VECTOR
    lda #>INPUT_BUFFER
    sta STRIN_VECTOR+1

    rts

; ****************************************************************************
; Function:     term_write_buffer
; Description:  write a character to the terminal buffer
; Input:        character in a
; Changes:      x
; ****************************************************************************
term_write_buffer:
    ldx KBD_WPTR
    sta TERM_BUFFER,x
    inc KBD_WPTR

    rts

; ****************************************************************************
; Function:     term_read_buffer
; Description:  read a character from the terminal buffer
; Input:        nothing
; Changes:      a, x
; ****************************************************************************
term_read_buffer:
    ldx KBD_RPTR
    lda TERM_BUFFER,x
    inc KBD_RPTR

    rts

; ****************************************************************************
; Function:     term_read
; Description:  Read a character from the terminal
; Input:        nothing
; Changes:      A
; Returns:      Character in A. 0 if no character
; Flags:        Carry set if character, clear otherwise
; ****************************************************************************
term_read:
    lda KBD_RPTR
    cmp KBD_WPTR
    bne @haveChar

    clc
    bra @done

@haveChar:
    phx
    jsr term_read_buffer
    plx

    pha
    jsr term_get_buffer_space
    cmp #$B0
    bcs @mostly_full
    lda ACIA_CMD
    ora #%00001000
    sta ACIA_CMD

@mostly_full:
    pla
    sec

@done:
    rts

; ****************************************************************************
; Function:     term_get_char
; Description:  Get a character from the terminal, wait if none
; Input:        nothing
; Changes:      A
; Returns:      Character in A
; Flags:        Carry set
; ****************************************************************************
term_get_char:
    jsr term_read
    bcc term_get_char

    rts

; ****************************************************************************
; Function:     term_get_string
; Description:  Get a string from the terminal, wait for CR
; Input:        nothing
; Changes:      A
; Returns:      asciiz string stored at INPUT_BUFFER
;               a => length of string
; Flags:        z set if length is zero
; ****************************************************************************
term_get_string:
    phy

    ldy #$00
@nextChar:
    jsr term_get_char

    cmp #$08            ; is it backspace?
    beq @doBackspace

    cmp #$7F            ; is it delete?
    bne @checkCRLF

@doBackspace:
    cpy #$00            ; is it at the beginning of the line?
    beq @nextChar

    dey
    lda #$08
    jsr term_write      ; echo backspace
    lda #$20
    jsr term_write
    lda #$08
    jsr term_write

    bra @nextChar

@checkCRLF:
    cmp #$0D            ; is it carriage return?
    beq @done

    cmp #$0A            ; is it line feed?
    beq @done

    cpy #INPUT_BUFFER_LEN-1 ; is the buffer full?
    bne @storeChar

    lda #$07            ; beep if buffer full
    jsr term_write
    bra @nextChar

@storeChar:
    sta (STRIN_VECTOR),y    ; add character to string buffer
    jsr term_write          ; echo character

    iny
    bra @nextChar

@done:
    lda #$00                ; null-terminate the string
    sta (STRIN_VECTOR),y

    jsr term_write_crlf     ; echo carriage-return/line-feed

    tya                     ; return length in a
    ply

    and #$FF                ; set zero if length is zero

    rts

; ****************************************************************************
; Function:     term_unread
; Description:  Unread a character from the terminal
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:        n,z
; ****************************************************************************
term_unread:
    dec KBD_RPTR
    rts

; ****************************************************************************
; Function:     term_haschar
; Description:  Check if terminal character is available
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:        Carry set if character, clear otherwise
;               n,z
; ****************************************************************************
term_haschar:
    lda KBD_RPTR
    cmp KBD_WPTR
    beq @noChar

    sec
    bra @done

@noChar:
    clc

@done:
    rts

; ****************************************************************************
; Function:     term_write
; Description:  Write a character to the terminal
; Input:        A - character to output
; Changes:      nothing
; Returns:      nothing
; Flags:
; ****************************************************************************
term_write:
    jsr acia_write
    rts

; ****************************************************************************
; Function:     term_write_string
; Description:  Write a string to the terminal
; Input:        x,y - address of string (x-lo,y-hi)
; Changes:      nothing
; Returns:      nothing
; Flags:
; ****************************************************************************
term_write_string:
    pha
    phy

    stx STROUT_VECTOR
    sty STROUT_VECTOR+1

    ldy #$00
@writeChar:
    lda (STROUT_VECTOR),y
    beq @done

    jsr term_write

    iny
    bra @writeChar

@done:
    ply
    pla

    rts

; ****************************************************************************
; Function:     term_write_crlf
; Description:  Write charriage-return/line-feed to terminal
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:
; ****************************************************************************
term_write_crlf:
    pha

    lda #$0D
    jsr term_write
    lda #$0A
    jsr term_write

    pla
    rts


; ****************************************************************************
; Function:     term_get_buffer_space
; Description:  Get amount of space used in the keyboard buffer
; Input:        nothing
; Changes:      A
; Flags:
; ****************************************************************************
term_get_buffer_space:
    lda KBD_WPTR
    sec
    sbc KBD_RPTR
    rts

; ****************************************************************************
; Function:     termIRQ
; Description:  Terminal interrupt handler
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; Flags:
; ****************************************************************************
termIRQ:
    bit ACIA_STATUS
    bpl @noIRQ
    lda ACIA_DATA
    jsr term_write_buffer
    jsr term_get_buffer_space
    cmp #$F0
    bcc @not_full
    lda ACIA_CMD
    and #%11110111
    sta ACIA_CMD

@not_full:
    ; nothing

@noIRQ:
    rts

