    .setcpu "W65C02"

    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "term.inc"

    .segment "BIOS"

    .importzp MON_JUMP_VECTOR, MON_ADDR_VECTOR, MON_HEX_BYTE_BUFFER, MON_HEX_ADDR_BUFFER
    .import term_get_string, term_write, term_write_crlf, term_write_string
    .import timer_sleep
    .export monitor
    .export writeHexByte

monitor:

cmdLoop:
    jsr mon_showprompt
    jsr term_get_string
    beq cmdLoop

    jmp process_command

process_command:
    ldx #$00

@nextCommandChar:
    cpx #INPUT_BUFFER_LEN
    beq @done

    lda #$00
    cmp INPUT_BUFFER,x
    beq @done

    lda #$20
    cmp INPUT_BUFFER,x
    bne @notSpace

    inx
    bra @nextCommandChar

@notSpace:
    lda INPUT_BUFFER,x
    jsr lookupCommand
    beq @badCommand

    jmp doCommand

@badCommand:
    phx

    ldx #<cmderrmsg
    ldy #>cmderrmsg
    jsr term_write_string

    plx

    lda INPUT_BUFFER,x
    jsr term_write
    jsr term_write_crlf
    jmp doHelpCommand

@done:
    jmp cmdLoop

lookupCommand:
    ldy #$00

@checkCommandChar:
    cmp commands,y
    bne @nextCommand
    lda #$01

    rts

@nextCommand:
    pha
    lda commands,y
    beq @endOfList

    pla

    iny
    iny
    iny
    bra @checkCommandChar

@endOfList:
    pla
    lda #$00

@done:
    rts

doCommand:
    iny
    lda commands,y
    sta MON_JUMP_VECTOR
    iny
    lda commands,y
    sta MON_JUMP_VECTOR+1
    jmp (MON_JUMP_VECTOR)

doWriteCommand:
    jsr skipToNextToken
    bcs @badAddress

    jsr getHexAddr
    bcc @writeBytes

@badAddress:
    ldx #<badaddrmsg
    ldy #>badaddrmsg
    jsr term_write_string

    jmp cmdLoop

@writeBytes:
    lda MON_HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda MON_HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

@getNextByte:
    jsr skipToNextToken
    bcs @done

    jsr getHexByte
    bcc @writeByte

    ldx #<badbytemsg
    ldy #>badbytemsg
    jsr term_write_string

    jmp cmdLoop

@writeByte:
    lda MON_HEX_BYTE_BUFFER
    sta (MON_ADDR_VECTOR)

    inc MON_ADDR_VECTOR
    bne @getNextByte
    inc MON_ADDR_VECTOR+1

    bra @getNextByte

@done:
    jmp cmdLoop

doMemCommand:
    jsr skipToNextToken
    bcs @haveAddr

    jsr getHexAddr
    bcc @gotAddr

@badAddress:
    ldx #<badaddrmsg
    ldy #>badaddrmsg
    jsr term_write_string

    bra @done

@gotAddr:
    lda MON_HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda MON_HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

@haveAddr:
    ldx #$10

@loopRow:
    ldy #$00

    lda MON_ADDR_VECTOR+1
    jsr writeHexByte
    lda MON_ADDR_VECTOR
    jsr writeHexByte

    lda #$20
    jsr term_write

@loopCol:
    lda (MON_ADDR_VECTOR),y
    jsr writeHexByte

    iny
    cpy #$10
    beq @endRow

    lda #$20
    jsr term_write

    cpy #$08
    bne @loopCol

    jsr term_write
    bra @loopCol

@endRow:
    lda #$20
    jsr term_write
    jsr term_write

    ldy #$00

@endRowLoop:
    lda (MON_ADDR_VECTOR),y
    cmp #$20
    bcs @endRow1
    lda #'.'

@endRow1:
    cmp #$80
    bcc @endRow2
    lda #'.'

@endRow2:
    jsr term_write
    iny
    cpy #$10
    bne @endRowLoop
    
    jsr term_write_crlf

    lda #$10
    clc
    adc MON_ADDR_VECTOR
    sta MON_ADDR_VECTOR
    lda #$00
    adc MON_ADDR_VECTOR+1
    sta MON_ADDR_VECTOR+1

    dex
    bne @loopRow

@done:
    jmp cmdLoop

doJumpCommand:
    jsr skipToNextToken
    bcs @badAddress

    jsr getHexAddr
    bcc @gotAddress

@badAddress:
    ldx #<badaddrmsg
    ldy #>badaddrmsg
    jsr term_write_string

    jmp cmdLoop

@gotAddress:
    jmp (MON_HEX_ADDR_BUFFER)

doBlinkCommand:
    ldy #10
@loop:
    jsr @blink
    lda #100
    jsr timer_sleep
    dey
    bne @loop
    jmp cmdLoop

@blink:
    phy
    phx
    ldx #<blinkmsg
    ldy #>blinkmsg
    jsr term_write_string
    plx
    ply
    rts

blinkmsg:
    .asciiz "blink\r\n"

doHelpCommand:
    ldx #<helpmsg
    ldy #>helpmsg
    jsr term_write_string
    jmp cmdLoop

mon_showprompt:
    ldx #$0
@L1:
    lda monprompt,x
    beq @L2
    jsr term_write
    inx
    bra @L1
@L2:
    rts

skipSpaces:
    cpx #INPUT_BUFFER_LEN
    beq @done

    lda INPUT_BUFFER,x
    beq @done

    cmp #$20
    bne @done

    inx
    bra skipSpaces

@done:
    rts

skipToSpace:
    cpx #INPUT_BUFFER_LEN
    beq @done

    lda INPUT_BUFFER,x
    beq @done

    cmp #$20
    beq @done

    inx
    bra skipToSpace

@done:
    rts

skipToNextToken:
    jsr skipToSpace

    cpx #INPUT_BUFFER_LEN
    beq @noToken

    lda INPUT_BUFFER,x
    beq @noToken

    jsr skipSpaces

    cpx #INPUT_BUFFER_LEN
    beq @noToken

    lda INPUT_BUFFER,x
    beq @noToken

    clc

    rts

@noToken:
    sec

@done:
    rts

getHexAddr:
    jsr getHexByte
    bcs @done

    lda MON_HEX_BYTE_BUFFER
    sta MON_HEX_ADDR_BUFFER+1

    inx
    cpx #INPUT_BUFFER_LEN
    bne @getNextByte

    sec
    rts

@getNextByte:
    jsr getHexByte
    bcs @done

    lda MON_HEX_BYTE_BUFFER
    sta MON_HEX_ADDR_BUFFER

@done:
    rts

getHexByte:
    cpx #INPUT_BUFFER_LEN
    beq @badByte

    lda INPUT_BUFFER,x
    beq @badByte

    jsr hexToDigit
    asl
    asl
    asl
    asl
    sta MON_HEX_BYTE_BUFFER

    inx
    cpx #INPUT_BUFFER_LEN
    beq @badByte

    lda INPUT_BUFFER,x
    beq @badByte

    jsr hexToDigit
    ora MON_HEX_BYTE_BUFFER
    sta MON_HEX_BYTE_BUFFER

    clc

    rts

@badByte:
    sec

    rts

hexToDigit:
    cmp #$47
    bcc @unAscii

    and #$DF

@unAscii:
    sec
    sbc #$30
    cmp #$0a
    bcc @done
    sbc #$07

@done:
    rts

writeHexByte:
    pha
    pha

    lsr
    lsr
    lsr
    lsr

    jsr byteToHexDigit
    jsr term_write

    pla
    and #$0f

    jsr byteToHexDigit
    jsr term_write

    pla
    rts

byteToHexDigit:
    cmp #$0a
    bcc @makeAscii

    clc
    adc #$07

@makeAscii:
    clc
    adc #$30

    rts

commands:
    .byte 'w'
    .word doWriteCommand
    .byte 'm'
    .word doMemCommand
    .byte 'j'
    .word doJumpCommand
    .byte 'b'
    .word doBlinkCommand
    .byte 'h'
    .word doHelpCommand
    .byte $00
    .word doHelpCommand

monprompt:
    .asciiz "==> "
cmderrmsg:
    .asciiz "*** invalid command - "
helpmsg:
    .byte "Commands:\r\n"
    .byte "  w addr bb [bb..]    write bytes at addr\r\n"
    .byte "  m [addr]            dump memory at [addr] or most recent addr\r\n"
    .byte "  j addr              jump (execute) code at addr\r\n"
    .byte "  h                   display this help text\r\n"
    .byte $00
badaddrmsg:
    .asciiz "bad or missing address"
badbytemsg:
    .asciiz "bad or missing byte"

