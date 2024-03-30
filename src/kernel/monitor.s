    .setcpu "65C02"

    .segment "KERNEL"

; ****************************************************************************
; Function:     KMONINIT
; Description:  Initialize monitor
; Input:        nothing
; Changes:      a
; Returns:      nothing
; Flags:        nothing
; ****************************************************************************
KMONINIT:
    stz MON_CMD_VECTOR
    stz MON_CMD_VECTOR+1
    stz MON_ADDR_VECTOR
    stz MON_ADDR_VECTOR+1
    stz BRK_SAVE_A
    stz BRK_SAVE_X
    stz BRK_SAVE_Y
    stz BRK_SAVE_SP
    stz BRK_PC_VECTOR
    stz BRK_PC_VECTOR+1
    stz BRK_STATUS_REG
    stz MON_FROM_BRK
    lda #$01
    sta MON_BP_ENABLE
    lda #$ff
    sta MON_COLD_START

    rts

; ****************************************************************************
; Function:     KMONITOR
; Description:  DaveMon for the DPuter
; ****************************************************************************
KMONITOR:
    cli

    lda MON_COLD_START
    beq @notCold

    lda #$00
    sta MON_COLD_START

    ; jmp $0500

@notCold:
    ; are we here due to a BRK instruction?
    lda MON_FROM_BRK
    beq @cmdLoop

    ; in a BRK - display status info
    jsr mDisplayBrkStatus
    lda BRK_PC_VECTOR
    sta MON_ADDR_VECTOR
    lda BRK_PC_VECTOR+1
    sta MON_ADDR_VECTOR+1
    stz MON_FROM_BRK

@cmdLoop:
    jsr mPutStr
    .asciiz "\n==> "

    jsr KGETSTR
    beq @cmdLoop

    jsr mProcessCommand
    
    bra @cmdLoop

; ----------------------------------------------------------------------------
; Process the current monitor command
; ----------------------------------------------------------------------------
mProcessCommand:
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
    jsr mLookupCommand
    beq @badCommand

    jsr mDoCommand

    rts

@badCommand:
    jsr mPutStr
    .asciiz "*** invalid command - "

    lda INPUT_BUFFER,x
    jsr KWRITETERM
    jsr KWRITECRLF
    jmp mDoHelpCommand

@done:
    rts

; ----------------------------------------------------------------------------
; Lookup entered command in the command table
; ----------------------------------------------------------------------------
mLookupCommand:
    ldy #$00

@checkCommandChar:
    cmp mCommands,y
    bne @nextCommand
    lda #$01

    rts

@nextCommand:
    pha
    lda mCommands,y
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

; ----------------------------------------------------------------------------
; Execute the command
; ----------------------------------------------------------------------------
mDoCommand:
    iny
    lda mCommands,y
    sta MON_CMD_VECTOR
    iny
    lda mCommands,y
    sta MON_CMD_VECTOR+1
    jmp (MON_CMD_VECTOR)


; ----------------------------------------------------------------------------
; Command table
; ----------------------------------------------------------------------------
mCommands:
    .byte 'q'
    .word mDoQuitCommand
    .byte 'w'
    .word mDoWriteCommand
    .byte 'm'
    .word mDoMemCommand
    .byte 'j'
    .word mDoJumpCommand
    .byte 'c'
    .word mDoContinueCommand
    .byte 'b'
    .word mDoBreakpointCommand
    .byte 'd'
    .word mDoDisassembleCommand
    .byte 'l'
    .word mDoLoadCommand
    .byte 'h'
    .word mDoHelpCommand
    .byte $00
    .word mDoHelpCommand

; ----------------------------------------------------------------------------
; Write value(s) to memory
; ----------------------------------------------------------------------------
mDoWriteCommand:
    jsr mSkipToNextToken
    bcs @badAddress

    jsr mGetHexAddr
    bcc @writeBytes

@badAddress:
    jsr mPutStr
    .asciiz "bad or missing address\n"

    rts

@writeBytes:
    lda HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

    ldy #$00

@getNextByte:
    jsr mSkipToNextToken
    bcs @done

    jsr mGetHexByte
    bcc @writeByte

    jsr mPutStr
    .asciiz "incomplete byte"

    rts

@writeByte:
    lda HEX_BYTE_BUFFER
    sta (MON_ADDR_VECTOR),y

    lda #$01
    clc
    adc MON_ADDR_VECTOR
    sta MON_ADDR_VECTOR
    lda #$00
    adc MON_ADDR_VECTOR+1
    sta MON_ADDR_VECTOR+1

    bra @getNextByte

@done:
    rts

; ----------------------------------------------------------------------------
; Do memory dump
; ----------------------------------------------------------------------------
mDoMemCommand:
    jsr mSkipToNextToken
    bcs @haveAddr

    jsr mGetHexAddr
    bcc @gotAddr

@badAddress:
    jsr mPutStr
    .asciiz "bad address\n"

    bra @done

@gotAddr:
    lda HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

@haveAddr:
    ldx #$10

@loopRow:
    ldy #$00

    lda MON_ADDR_VECTOR+1
    jsr mWriteHexByte
    lda MON_ADDR_VECTOR
    jsr mWriteHexByte

    lda #$20
    jsr KWRITETERM

@loopCol:
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte

    iny
    cpy #$10
    beq @endRow

    lda #$20
    jsr KWRITETERM

    cpy #$08
    bne @loopCol

    jsr KWRITETERM
    bra @loopCol

@endRow:
    lda #$20
    jsr KWRITETERM
    jsr KWRITETERM

    ldy #$00

@endRowLoop:
    lda (MON_ADDR_VECTOR),y
    cmp #32
    bcs @endRow1
    lda #'.'

@endRow1:
    cmp #128
    bcc @endRow2
    lda #'.'

@endRow2:
    jsr KWRITETERM
    iny
    cpy #$10
    beq @doneRow
    bra @endRowLoop

@doneRow:
    jsr KWRITECRLF

    lda #$10
    clc
    adc MON_ADDR_VECTOR
    sta MON_ADDR_VECTOR
    lda #$00
    adc MON_ADDR_VECTOR+1
    sta MON_ADDR_VECTOR+1

    dex
    beq @done

    bra @loopRow

@done:
    rts

; ----------------------------------------------------------------------------
; Jump to specified address
; ----------------------------------------------------------------------------
mDoJumpCommand:
    jsr mSkipToNextToken
    bcs @badAddress

    jsr mGetHexAddr
    bcc @gotAddress

@badAddress:
    jsr mPutStr
    .asciiz "bad or missing address\n"

    rts

@gotAddress:
    lda HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

    jmp (MON_ADDR_VECTOR)

; ----------------------------------------------------------------------------
; Continue from breakpoint
; ----------------------------------------------------------------------------
mDoContinueCommand:
    clc
    lda BRK_PC_VECTOR
    adc #$02
    sta BRK_PC_VECTOR
    lda BRK_PC_VECTOR+1
    adc #$00
    sta BRK_PC_VECTOR+1

    ldx BRK_SAVE_SP
    txs
    pla                     ; SP on BRK was pointing after status and return addr
    pla                     ; so throw away the junk that is there
    pla

    lda BRK_PC_VECTOR+1
    pha
    lda BRK_PC_VECTOR
    pha
    lda BRK_STATUS_REG
    pha

    lda BRK_SAVE_A
    ldx BRK_SAVE_X
    ldy BRK_SAVE_Y

    rti

; ----------------------------------------------------------------------------
; Quit. (Halt the cpu)
; ----------------------------------------------------------------------------
mDoQuitCommand:
    stp

; ----------------------------------------------------------------------------
; Display monitor help
; ----------------------------------------------------------------------------
mDoHelpCommand:
    jsr mPutStr
    .byte   "Commands:\n"
    .byte   "  q                   quit\n"
    .byte   "  w addr bb [bb..]    write bytes at addr\n"
    .byte   "  m [addr]            dump memory at [addr] or most recent addr\n"
    .byte   "  j addr              jump to address\n"
    .byte   "  c                   continue (from breakpoint)\n"
    .byte   "  b                   toggle breakpoints\n"
    .byte   "  d [addr]            disassemble 20 instructions at [addr] or most recent addr\n"
    .byte   "  l filename          load binary file\n"
    .byte   "                      assumes load address in first 2 bytes of file\n"
    .asciiz "  h                   display this help text\n"
    rts

; ----------------------------------------------------------------------------
; Toggle breakpoints
; ----------------------------------------------------------------------------
mDoBreakpointCommand:
    lda #$01
    eor MON_BP_ENABLE
    sta MON_BP_ENABLE

    jsr mPutStr
    .asciiz "breakpoints "

    lda MON_BP_ENABLE
    beq @disabled

    jsr mPutStr
    .asciiz "enabled\n"

    rts

@disabled:
    jsr mPutStr
    .asciiz "disabled\n"

    rts

; ----------------------------------------------------------------------------
; Disassembler
; ----------------------------------------------------------------------------
mDoDisassembleCommand:
    jsr mSkipToNextToken
    bcs @haveAddr

    jsr mGetHexAddr
    bcc @gotAddr

    jsr mPutStr
    .asciiz "bad address\n"

    rts

@gotAddr:
    lda HEX_ADDR_BUFFER
    sta MON_ADDR_VECTOR
    lda HEX_ADDR_BUFFER+1
    sta MON_ADDR_VECTOR+1

@haveAddr:
    ldx #$14

@loop:
    phx
    jsr mDisassemble
    plx
    dex
    beq @done

    clc
    adc MON_ADDR_VECTOR
    sta MON_ADDR_VECTOR
    lda #$00
    adc MON_ADDR_VECTOR+1
    sta MON_ADDR_VECTOR+1

    bra @loop

@done:
    rts

; ----------------------------------------------------------------------------
; Disassemble one instruction
; ----------------------------------------------------------------------------
mDisassemble:
    lda MON_ADDR_VECTOR+1       ; output pc
    jsr mWriteHexByte
    lda MON_ADDR_VECTOR
    jsr mWriteHexByte
    jsr mWriteSpace

    lda (MON_ADDR_VECTOR)       ; save opcode
    sta MON_OPCODE

    sta MON_OPCODE_VECTOR       ; calculate offset into opcode table
    stz MON_OPCODE_VECTOR+1

    clc
    asl MON_OPCODE_VECTOR
    rol MON_OPCODE_VECTOR+1     ; opcode * 2

    clc
    lda MON_OPCODE
    adc MON_OPCODE_VECTOR
    sta MON_OPCODE_VECTOR
    lda #$0
    adc MON_OPCODE_VECTOR+1
    sta MON_OPCODE_VECTOR+1     ; opcode * 3

    clc
    asl MON_OPCODE_VECTOR
    rol MON_OPCODE_VECTOR+1     ; opcode * 6

    clc                         ; calculate absolute address in opcode tbl
    lda #<mOpcodes
    adc MON_OPCODE_VECTOR
    sta MON_OPCODE_VECTOR
    lda #>mOpcodes
    adc MON_OPCODE_VECTOR+1
    sta MON_OPCODE_VECTOR+1

    ldy #$05
    lda (MON_OPCODE_VECTOR),y   ; instruction length
    tax

    ldy #$00
    lda (MON_ADDR_VECTOR),y     ; opcode
    jsr mWriteHexByte
    jsr mWriteSpace

    dex
    beq @bytesDone

    iny
    lda (MON_ADDR_VECTOR),y     ; operand 1
    jsr mWriteHexByte
    jsr mWriteSpace

    dex
    beq @bytesDone

    iny
    lda (MON_ADDR_VECTOR),y     ; operand 2
    jsr mWriteHexByte
    jsr mWriteSpace

@bytesDone:
    ldy #$05                    ; add spaces to account for < 2 operands
    lda #$03
    sec
    sbc (MON_OPCODE_VECTOR),y
    tax
    beq @spacesDone

@spacesLoop:
    jsr mWriteSpace
    jsr mWriteSpace
    jsr mWriteSpace
    dex
    bne @spacesLoop

@spacesDone:
    ldx MON_OPCODE_VECTOR       ; output instruction mnemonic
    ldy MON_OPCODE_VECTOR+1
    jsr KWRITESTR
    jsr mWriteSpace

    ldy #$04
    lda (MON_OPCODE_VECTOR),y   ; get addressing mode
    jsr @handleAddrMode         ; output instruction operands
    iny
    lda (MON_OPCODE_VECTOR),y   ; get instruction length
    jmp KWRITECRLF

@handleAddrMode:
    cmp #AM_ACCUMULATOR
    bne @checkAbsolute

    ; xxx
    jmp @handleAddrModeDone

@checkAbsolute:
    cmp #AM_ABSOLUTE
    bne @checkAbsoluteX

    ; xxx $hilo
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$02
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    dey
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte

    ply
    jmp @handleAddrModeDone

@checkAbsoluteX:
    cmp #AM_ABSOLUTE_X
    bne @checkAbsoluteY

    ; xxx $hilo,x
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$02
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    dey
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #','
    jsr KWRITETERM
    lda #'x'
    jsr KWRITETERM

    ply
    jmp @handleAddrModeDone

@checkAbsoluteY:
    cmp #AM_ABSOLUTE_Y
    bne @checkImmediate

    ; xxx $hilo,y
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$02
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    dey
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #','
    jsr KWRITETERM
    lda #'y'
    jsr KWRITETERM

    ply
    jmp @handleAddrModeDone

@checkImmediate:
    cmp #AM_IMMEDIATE
    bne @checkImplied

    ; xxx #$bb
    phy

    lda #'#'
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte

    ply
    jmp @handleAddrModeDone

@checkImplied:
    cmp #AM_IMPLIED
    bne @checkIndirect

    ; xxx
    jmp @handleAddrModeDone

@checkIndirect:
    cmp #AM_INDIRECT
    bne @checkXIdxIndir

    ; xxx ($hilo)
    phy

    lda #'('
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$02
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    dey
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #')'
    jsr KWRITETERM

    ply
    jmp @handleAddrModeDone

@checkXIdxIndir:
    cmp #AM_X_IDX_INDIR
    bne @checkIndirYIdx

    ; xxx ($zp,x)
    phy

    lda #'('
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #','
    jsr KWRITETERM
    lda #'x'
    jsr KWRITETERM
    lda #')'
    jsr KWRITETERM

    ply
    jmp @handleAddrModeDone

@checkIndirYIdx:
    cmp #AM_INDIR_Y_IDX
    bne @checkRelative

    ; xxx ($zp),y
    phy

    lda #'('
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #')'
    jsr KWRITETERM
    lda #','
    jsr KWRITETERM
    lda #'y'
    jsr KWRITETERM

    ply
    jmp @handleAddrModeDone

@checkRelative:
    cmp #AM_RELATIVE
    bne @checkZeroPage

    ; xxx $ro => xxx $hilo
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    bpl @forwardBranch

    eor #$ff				; backard relative branch
    clc
    adc #$1
    sec
    sta T1_VECTOR
    lda MON_ADDR_VECTOR
    sbc T1_VECTOR
    sta T1_VECTOR
    lda MON_ADDR_VECTOR+1
    sbc #$0
    sta T1_VECTOR+1
    bra @adjustRelativeVector

@forwardBranch:
    clc						; forward relative branch
    adc MON_ADDR_VECTOR
    sta T1_VECTOR
    lda MON_ADDR_VECTOR+1
    adc #$0
    sta T1_VECTOR+1

@adjustRelativeVector:
    clc						; add 2 to get correct branch addr
    lda T1_VECTOR
    adc #$02
    sta T1_VECTOR
    lda T1_VECTOR+1
    adc #$00
    jsr mWriteHexByte
    lda T1_VECTOR
    jsr mWriteHexByte

@didRelative:
    ply
    jmp @handleAddrModeDone

@checkZeroPage:
    cmp #AM_ZEROPAGE
    bne @checkZeroPageX

    ; xxx $zp
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte

    ply
    jmp @handleAddrModeDone

@checkZeroPageX:
    cmp #AM_ZEROPAGE_X
    bne @checkZeroPageY

    ; xxx $zp,x
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #','
    jsr KWRITETERM
    lda #'x'
    jsr KWRITETERM

    ply
    bra @handleAddrModeDone

@checkZeroPageY:
    cmp #AM_ZEROPAGE_Y
    bne @checkAbsIdxInd

    ; xxx $zp,y
    phy

    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #','
    jsr KWRITETERM
    lda #'y'
    jsr KWRITETERM

    ply
    bra @handleAddrModeDone

@checkAbsIdxInd:
    cmp #AM_ABS_IDX_IND
    bne @checkZeroPageInd

    ; xxx ($hilo),x
    phy

    lda #'('
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$02
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    dey
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #')'
    jsr KWRITETERM
    lda #','
    jsr KWRITETERM
    lda #'x'
    jsr KWRITETERM

    ply
    bra @handleAddrModeDone

@checkZeroPageInd:
    cmp #AM_ZEROPAGE_IND
    bne @unknownAddrMode

    ; xxx ($zp)
    phy

    lda #'('
    jsr KWRITETERM
    lda #'$'
    jsr KWRITETERM
    ldy #$01
    lda (MON_ADDR_VECTOR),y
    jsr mWriteHexByte
    lda #')'
    jsr KWRITETERM

    ply
    bra @handleAddrModeDone

@unknownAddrMode:
    jsr mPutStr
    .asciiz "addr mode "
    jsr mWriteHexByte

@handleAddrModeDone:
    rts

; Addressing modes
AM_ACCUMULATOR      = 1
AM_ABSOLUTE         = 2
AM_ABSOLUTE_X       = 3
AM_ABSOLUTE_Y       = 4
AM_IMMEDIATE        = 5
AM_IMPLIED          = 6
AM_INDIRECT         = 7
AM_X_IDX_INDIR      = 8
AM_INDIR_Y_IDX      = 9
AM_RELATIVE         = 10
AM_ZEROPAGE         = 11
AM_ZEROPAGE_X       = 12
AM_ZEROPAGE_Y       = 13
AM_ABS_IDX_IND      = 14
AM_ZEROPAGE_IND     = 15

mOpcodes:
    .byte "brk",$00,AM_IMMEDIATE,2
    .byte "ora",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_ZEROPAGE,2
    .byte "asl",$00,AM_ZEROPAGE,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "php",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_IMMEDIATE,2
    .byte "asl",$00,AM_ACCUMULATOR,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_ABSOLUTE,3
    .byte "asl",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bpl",$00,AM_RELATIVE,2
    .byte "ora",$00,AM_INDIR_Y_IDX,2
    .byte "ora",$00,AM_ZEROPAGE_IND,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_ZEROPAGE_X,2
    .byte "asl",$00,AM_ZEROPAGE_X,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "clc",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_ABSOLUTE_Y,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ora",$00,AM_ABSOLUTE_X,3
    .byte "asl",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "jsr",$00,AM_ABSOLUTE,3
    .byte "and",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bit",$00,AM_ZEROPAGE,2
    .byte "and",$00,AM_ZEROPAGE,2
    .byte "rol",$00,AM_ZEROPAGE,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "plp",$00,AM_IMPLIED,1
    .byte "and",$00,AM_IMMEDIATE,2
    .byte "rol",$00,AM_ACCUMULATOR,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bit",$00,AM_ABSOLUTE,3
    .byte "abs",$00,AM_ABSOLUTE,3
    .byte "rol",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bmi",$00,AM_RELATIVE,2
    .byte "and",$00,AM_INDIR_Y_IDX,2
    .byte "and",$00,AM_ZEROPAGE_IND,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bit",$00,AM_ZEROPAGE_X,2
    .byte "and",$00,AM_ZEROPAGE_X,2
    .byte "rol",$00,AM_ZEROPAGE_X,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "sec",$00,AM_IMPLIED,1
    .byte "and",$00,AM_ABSOLUTE_Y,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bit",$00,AM_ABSOLUTE_X,3
    .byte "and",$00,AM_ABSOLUTE_X,3
    .byte "rol",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "rti",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_ZEROPAGE,2
    .byte "lsr",$00,AM_ZEROPAGE,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "pha",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_IMMEDIATE,2
    .byte "lsr",$00,AM_ACCUMULATOR,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "jmp",$00,AM_ABSOLUTE,3
    .byte "eor",$00,AM_ABSOLUTE,3
    .byte "lsr",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bvc",$00,AM_RELATIVE,2
    .byte "eor",$00,AM_INDIR_Y_IDX,2
    .byte "eor",$00,AM_ZEROPAGE_IND,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_ZEROPAGE_X,2
    .byte "lsr",$00,AM_ZEROPAGE_X,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "cli",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_ABSOLUTE_Y,3
    .byte "phy",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "eor",$00,AM_ABSOLUTE_X,3
    .byte "lsr",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "rts",$00,AM_IMPLIED,1
    .byte "adc",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "stz",$00,AM_ZEROPAGE,2
    .byte "adc",$00,AM_ZEROPAGE,2
    .byte "ror",$00,AM_ZEROPAGE,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "pla",$00,AM_IMPLIED,1
    .byte "adc",$00,AM_IMMEDIATE,2
    .byte "ror",$00,AM_ACCUMULATOR,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "jmp",$00,AM_INDIRECT,3
    .byte "adc",$00,AM_ABSOLUTE,3
    .byte "ror",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bvs",$00,AM_RELATIVE,2
    .byte "adc",$00,AM_INDIR_Y_IDX,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "stz",$00,AM_ZEROPAGE_X,2
    .byte "adc",$00,AM_ZEROPAGE_X,2
    .byte "ror",$00,AM_ZEROPAGE_X,2
    .byte "rmb",$00,AM_ZEROPAGE,2
    .byte "sei",$00,AM_IMPLIED,1
    .byte "adc",$00,AM_ABSOLUTE_Y,3
    .byte "ply",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "jmp",$00,AM_ABS_IDX_IND,3
    .byte "adc",$00,AM_ABSOLUTE_X,3
    .byte "ror",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bra",$00,AM_RELATIVE,2
    .byte "sta",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "sty",$00,AM_ZEROPAGE,2
    .byte "sta",$00,AM_ZEROPAGE,2
    .byte "stx",$00,AM_ZEROPAGE,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "dey",$00,AM_IMPLIED,1
    .byte "bit",$00,AM_IMMEDIATE,2
    .byte "txa",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "sty",$00,AM_ABSOLUTE,3
    .byte "sta",$00,AM_ABSOLUTE,3
    .byte "stx",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bcc",$00,AM_RELATIVE,2
    .byte "sta",$00,AM_INDIR_Y_IDX,2
    .byte "sta",$00,AM_ZEROPAGE_IND,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "sty",$00,AM_ZEROPAGE_X,2
    .byte "sta",$00,AM_ZEROPAGE_X,2
    .byte "stx",$00,AM_ZEROPAGE_Y,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "tya",$00,AM_IMPLIED,1
    .byte "sta",$00,AM_ABSOLUTE_Y,3
    .byte "txs",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "stz",$00,AM_ABSOLUTE,3
    .byte "sta",$00,AM_ABSOLUTE_X,3
    .byte "stz",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ldy",$00,AM_IMMEDIATE,2
    .byte "lda",$00,AM_X_IDX_INDIR,2
    .byte "ldx",$00,AM_IMMEDIATE,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ldy",$00,AM_ZEROPAGE,2
    .byte "lda",$00,AM_ZEROPAGE,2
    .byte "ldx",$00,AM_ZEROPAGE,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "tay",$00,AM_IMPLIED,1
    .byte "lda",$00,AM_IMMEDIATE,2
    .byte "tax",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ldy",$00,AM_ABSOLUTE,3
    .byte "lda",$00,AM_ABSOLUTE,3
    .byte "ldx",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bcs",$00,AM_RELATIVE,2
    .byte "lda",$00,AM_INDIR_Y_IDX,2
    .byte "lda",$00,AM_ZEROPAGE_IND,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ldy",$00,AM_ZEROPAGE_X,2
    .byte "lda",$00,AM_ZEROPAGE_X,2
    .byte "ldx",$00,AM_ZEROPAGE_Y,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "clv",$00,AM_IMPLIED,1
    .byte "lda",$00,AM_ABSOLUTE_Y,3
    .byte "tsx",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "ldy",$00,AM_ABSOLUTE_X,3
    .byte "lda",$00,AM_ABSOLUTE_X,3
    .byte "ldx",$00,AM_ABSOLUTE_Y,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpy",$00,AM_IMMEDIATE,2
    .byte "cmp",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpy",$00,AM_ZEROPAGE,2
    .byte "cmp",$00,AM_ZEROPAGE,2
    .byte "dec",$00,AM_ZEROPAGE,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "iny",$00,AM_IMPLIED,1
    .byte "cmp",$00,AM_IMMEDIATE,2
    .byte "dex",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpy",$00,AM_ABSOLUTE,3
    .byte "cmp",$00,AM_ABSOLUTE,3
    .byte "dec",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "bne",$00,AM_RELATIVE,2
    .byte "cmp",$00,AM_INDIR_Y_IDX,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cmp",$00,AM_ZEROPAGE_X,2
    .byte "dec",$00,AM_ZEROPAGE_X,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "cld",$00,AM_IMPLIED,1
    .byte "cmp",$00,AM_ABSOLUTE_Y,3
    .byte "phx",$00,AM_IMPLIED,1
    .byte "STP",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cmp",$00,AM_ABSOLUTE_X,3
    .byte "dec",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpx",$00,AM_IMMEDIATE,2
    .byte "sbc",$00,AM_X_IDX_INDIR,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpx",$00,AM_ZEROPAGE,2
    .byte "sbc",$00,AM_ZEROPAGE,2
    .byte "inc",$00,AM_ZEROPAGE,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "inx",$00,AM_IMPLIED,1
    .byte "sbc",$00,AM_IMMEDIATE,2
    .byte "nop",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "cpx",$00,AM_ABSOLUTE,3
    .byte "sbc",$00,AM_ABSOLUTE,3
    .byte "inc",$00,AM_ABSOLUTE,3
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "beq",$00,AM_RELATIVE,2
    .byte "sbc",$00,AM_INDIR_Y_IDX,2
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "sbc",$00,AM_ZEROPAGE_X,2
    .byte "inc",$00,AM_ZEROPAGE_X,2
    .byte "smb",$00,AM_ZEROPAGE,2
    .byte "sed",$00,AM_IMPLIED,1
    .byte "sbc",$00,AM_ABSOLUTE_Y,3
    .byte "plx",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "BAD",$00,AM_IMPLIED,1
    .byte "sbc",$00,AM_ABSOLUTE_X,3
    .byte "inc",$00,AM_ABSOLUTE_X,3
    .byte "BAD",$00,AM_IMPLIED,1

; ----------------------------------------------------------------------------
; Load file to memory
; ----------------------------------------------------------------------------
mDoLoadCommand:
    jsr mSkipToNextToken
    bcc @openFile

    jmp mDoHelpCommand


@openFile:
    txa
    clc
    adc #<INPUT_BUFFER
    tax
    lda #0
    adc #>INPUT_BUFFER
    tay

    jsr mPutStr
    .asciiz "loading file "

    jsr KWRITESTR
    jsr KWRITECRLF

    lda #FILEIO_MODE_READ
    ora #FILEIO_MODE_BIN
    jsr KFILEOPEN
    bcc @loadFileAddr

    jsr mPutStr
    .asciiz "Could not open file\n"

    rts

@loadFileAddr:
    sta MON_FD

    jsr KFILEREAD
    bcs @loadFileError

    sta T1_VECTOR

    lda MON_FD
    jsr KFILEREAD
    bcs @loadFileError

    sta T1_VECTOR+1

    jsr mPutStr
    .asciiz "> "

    lda T1_VECTOR+1
    jsr mWriteHexByte
    lda T1_VECTOR
    jsr mWriteHexByte
    jsr KWRITECRLF

    bra @loadFileData

@loadFileError:
    jsr mPutStr
    .asciiz "Error reading load address\n"

    lda MON_FD
    jsr KFILECLOSE

    rts

@loadFileData:
    lda MON_FD
    jsr KFILEREAD
    bcs @loadFileDataDone

    sta (T1_VECTOR)

    inc T1_VECTOR
    bne @loadFileData
    inc T1_VECTOR+1

    bra @loadFileData

@loadFileDataDone:
    lda FILEIO::STATUS
    cmp #FILEIO_STATUS_EOF
    beq @closeFile

    jsr mPutStr
    .asciiz "Error reading file data\n"

@closeFile:
    lda MON_FD
    jsr KFILECLOSE

    rts

; ----------------------------------------------------------------------------
; Display CPU status on BRK
; ----------------------------------------------------------------------------
mDisplayBrkStatus:
    jsr mPutStr
    .byte   "\nbreakpoint"
    .byte   "\nPC    BYTES   A   X   Y   SP  NVuBDIZC"
    .asciiz "\n----  ------  --  --  --  --  --------\n"
		
    lda BRK_PC_VECTOR+1
    jsr mWriteHexByte
    lda BRK_PC_VECTOR
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace

    ldy #$00
    lda (BRK_PC_VECTOR),y
    jsr mWriteHexByte

    jsr mWriteSpace

    iny
    lda (BRK_PC_VECTOR),y
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace
    jsr mWriteSpace

    lda BRK_SAVE_A
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace

    lda BRK_SAVE_X
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace

    lda BRK_SAVE_Y
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace

    lda BRK_SAVE_SP
    jsr mWriteHexByte

    jsr mWriteSpace
    jsr mWriteSpace

    lda BRK_STATUS_REG
    jsr mWriteBinaryByte

    jmp KWRITECRLF

; ----------------------------------------------------------------------------
; Output a string whose address is on the return stack to the terminal.
; Update return address to allow rts to return to the address after the string.
; ----------------------------------------------------------------------------
mPutStr:
    stx XSAVE2
    sty YSAVE2
    sta ASAVE2

    pla             ; get return address into zero-page
    sta WIMT_VECTOR
    pla
    sta WIMT_VECTOR+1

    ldy #$00
@nextChar:
    inc WIMT_VECTOR
    bne @writeChar
    inc WIMT_VECTOR+1

@writeChar:
    lda (WIMT_VECTOR),y
    beq @done
    jsr KWRITETERM

    bra @nextChar

@done:
    lda WIMT_VECTOR+1
    pha
    lda WIMT_VECTOR
    pha

    ldx XSAVE2
    ldy YSAVE2
    lda ASAVE2

    rts

; ----------------------------------------------------------------------------
; Output a space to the terminal
; ----------------------------------------------------------------------------
mWriteSpace:
    lda #$20
    jmp KWRITETERM

; ----------------------------------------------------------------------------
; Output a byte as hex to the terminal
; ----------------------------------------------------------------------------
mWriteHexByte:
    pha
    pha

    lsr
    lsr
    lsr
    lsr

    jsr mByteToHexDigit
    jsr KWRITETERM

    pla
    and #$0f

    jsr mByteToHexDigit
    jsr KWRITETERM

    pla
    rts

; ----------------------------------------------------------------------------
; Convert byte in A to a hex digit in ascii
; ----------------------------------------------------------------------------
mByteToHexDigit:
    cmp #$0a
    bcc @makeAscii

    clc
    adc #$07

@makeAscii:
    clc
    adc #$30

    rts

; ----------------------------------------------------------------------------
; Output the value in A as binary to the terminal
; ----------------------------------------------------------------------------
mWriteBinaryByte:
    pha
    phx

    clc
    ldx #$08
@nextDigit:
    asl
    pha

    bcc @isZero

    lda #'1'
    bra @outputDigit

@isZero:
    lda #'0'

@outputDigit:
    jsr KWRITETERM

    pla

    dex
    bne @nextDigit

    plx
    pla

    rts

; ----------------------------------------------------------------------------
; Skip spaces in input buffer
; ----------------------------------------------------------------------------
mSkipSpaces:
    cpx #INPUT_BUFFER_LEN
    beq @done

    lda INPUT_BUFFER,x
    beq @done

    cmp #$20
    bne @done

    inx
    bra mSkipSpaces

@done:
    rts

; ----------------------------------------------------------------------------
; Skip to next space in input buffer
; ----------------------------------------------------------------------------
mSkipToSpace:
    cpx #INPUT_BUFFER_LEN
    beq @done

    lda INPUT_BUFFER,x
    beq @done

    cmp #$20
    beq @done

    inx
    bra mSkipToSpace

@done:
    rts

; ----------------------------------------------------------------------------
; Skip to next token in input buffer
; ----------------------------------------------------------------------------
mSkipToNextToken:
    jsr mSkipToSpace

    cpx #INPUT_BUFFER_LEN
    beq @noToken

    lda INPUT_BUFFER,x
    beq @noToken

    jsr mSkipSpaces

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

; ----------------------------------------------------------------------------
; Get hex address from input buffer
; ----------------------------------------------------------------------------
mGetHexAddr:
    jsr mGetHexByte
    bcs @done

    lda HEX_BYTE_BUFFER
    sta HEX_ADDR_BUFFER+1

    inx
    cpx #INPUT_BUFFER_LEN
    bne @getNextByte

    sec
    rts

@getNextByte:
    jsr mGetHexByte
    bcs @done

    lda HEX_BYTE_BUFFER
    sta HEX_ADDR_BUFFER

@done:
    rts

; ----------------------------------------------------------------------------
; Get hex byte from input buffer
; ----------------------------------------------------------------------------
mGetHexByte:
    cpx #INPUT_BUFFER_LEN
    beq @badByte

    lda INPUT_BUFFER,x
    beq @badByte

    jsr mHexToDigit
    asl
    asl
    asl
    asl
    sta HEX_BYTE_BUFFER

    inx
    cpx #INPUT_BUFFER_LEN
    beq @badByte

    lda INPUT_BUFFER,x
    beq @badByte

    jsr mHexToDigit
    ora HEX_BYTE_BUFFER
    sta HEX_BYTE_BUFFER

    clc

    rts

@badByte:
    sec

@done:
    rts

; ----------------------------------------------------------------------------
; Convert hex nibble to a digit
; ----------------------------------------------------------------------------
mHexToDigit:
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

