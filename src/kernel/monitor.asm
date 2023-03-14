;*****************************************************************************
; monitor.asm
; 	(included by kernel.asm)
;*****************************************************************************

MONITOR .block
		cli

		lda MON_COLD_START
		beq monitor1
		lda #0
		sta MON_COLD_START
		;jmp $0200

monitor1
		; are we here due to BRK instruction?
		lda MON_FROM_BRK
		beq cmdLoop

		; in a BRK, so display CPU status
		jsr displayBrkStatus
		lda BRK_PC_VECTOR
		sta MON_ADDR_VECTOR
		lda BRK_PC_VECTOR+1
		sta MON_ADDR_VECTOR+1
		stz MON_FROM_BRK

cmdLoop
		jsr MPutStr
		.null $0a,"==> "

		jsr KGETSTR
		beq cmdLoop

		jsr processCommand

		bra cmdLoop

displayBrkStatus .proc
		jsr MPutStr
		.text $0a,"breakpoint"
		.text $0a,"PC    BYTES   A   X   Y   SP  NVuBDIZC"
		.null $0a,"----  ------  --  --  --  --  --------",$0a
		
		lda BRK_PC_VECTOR+1
		jsr MWriteHexByte
		lda BRK_PC_VECTOR
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace

		ldy #$00
		lda (BRK_PC_VECTOR),y
		jsr MWriteHexByte

		jsr writeSpace

		iny
		lda (BRK_PC_VECTOR),y
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace
		jsr writeSpace

		lda BRK_SAVE_A
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace

		lda BRK_SAVE_X
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace

		lda BRK_SAVE_Y
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace

		lda BRK_SAVE_SP
		jsr MWriteHexByte

		jsr writeSpace
		jsr writeSpace

		lda BRK_STATUS_REG
		jsr MWriteBinaryByte

		jmp KWRITECRLF
		.endproc


processCommand .proc
		ldx #$00

nextCommandChar
		cpx #INPUT_BUFFER_LEN
		beq done

		lda #$00
		cmp INPUT_BUFFER,x
		beq done

		lda #$20
		cmp INPUT_BUFFER,x
		bne notSpace

		inx
		bra nextCommandChar

notSpace
		lda INPUT_BUFFER,x
		jsr lookupCommand
		beq badCommand

		jsr doCommand

		bra done

badCommand
		jsr MPutStr
		.null "*** invalid command - "

		lda INPUT_BUFFER,x
		jsr KWRITETERM
		jsr KWRITECRLF
		jmp doHelp

done
		rts

doCommand
		iny
		lda commands,y
		sta MON_CMD_VECTOR
		iny
		lda commands,y
		sta MON_CMD_VECTOR+1
		jmp (MON_CMD_VECTOR)

		rts

lookupCommand .proc
		ldy #$00

checkCommandChar
		cmp commands,y
		bne nextCommand
		lda #$01
		bra done

nextCommand
		pha
		lda commands,y
		beq endOfList

		pla

		iny
		iny
		iny
		bra checkCommandChar

endOfList
		pla
		lda #$00

done
		rts
		.endproc

commands
		.byte 'q'
		.word doQuit
		.byte 'w'
		.word doWrite
		.byte 'm'
		.word doMem
		.byte 'j'
		.word doJump
		.byte 'c'
		.word doContinue
		.byte 'b'
		.word doBreakpoint
		.byte 'd'
		.word doDisassemble
		.byte 'l'
		.word doLoad
		.byte 't'
		.word doTest
		.byte 'h'
		.word doHelp
		.byte $00
		.word doHelp

doQuit .proc
		hlt
		.endproc

doWrite .proc
		jsr skipToNextToken
		bcs doWriteBadAddress

		jsr getHexAddr
		bcc doWrite2

doWriteBadAddress
		jsr MPutStr
		.null "bad or missing address",$0a

		bra doWriteEnd

doWrite2
		lda HEX_ADDR_BUFFER
		sta MON_ADDR_VECTOR
		lda HEX_ADDR_BUFFER+1
		sta MON_ADDR_VECTOR+1

		ldy #$00

doWrite3
		jsr skipToNextToken
		bcs doWriteEnd

		jsr getHexByte
		bcc doWrite4

		jsr MPutStr
		.null "incomplete byte"

		bra doWriteEnd

doWrite4
		lda HEX_BYTE_BUFFER
		sta (MON_ADDR_VECTOR),y

		lda #$01
		clc
		adc MON_ADDR_VECTOR
		sta MON_ADDR_VECTOR
		lda #$00
		adc MON_ADDR_VECTOR+1
		sta MON_ADDR_VECTOR+1

		bra doWrite3

doWriteEnd
		rts
		.endproc

doMem .proc
		jsr skipToNextToken
		bcs doMemHaveAddr

		jsr getHexAddr
		bcc doMemGotAddr

doMemBadAddress
		jsr MPutStr
		.null "bad address",$0a

		bra doMemEnd

doMemGotAddr
		lda HEX_ADDR_BUFFER
		sta MON_ADDR_VECTOR
		lda HEX_ADDR_BUFFER+1
		sta MON_ADDR_VECTOR+1

doMemHaveAddr
		ldx #$10

doMemLoopRow
		ldy #$00

		lda MON_ADDR_VECTOR+1
		jsr MWriteHexByte
		lda MON_ADDR_VECTOR
		jsr MWriteHexByte

		lda #$20
		jsr KWRITETERM

doMemLoopCol
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte

		iny
		cpy #$10
		beq doMemEndRow

		lda #$20
		jsr KWRITETERM

		cpy #$08
		bne doMemLoopCol

		jsr KWRITETERM
		bra doMemLoopCol

doMemEndRow
		lda #$20
		ldy #$00
		jsr KWRITETERM
		jsr KWRITETERM
doMemEndRowLoop
		lda (MON_ADDR_VECTOR),y
		cpa #32
		bge doMemEndRow1
		lda #'.'
doMemEndRow1
		cpa #128
		blt domemEndRow2
		lda #'.'
doMemEndRow2
		jsr KWRITETERM
		iny
		cpy #$10
		beq doMemDoneRow
		bra doMemEndRowLoop
doMemDoneRow
		jsr KWRITECRLF

		lda #$10
		clc
		adc MON_ADDR_VECTOR
		sta MON_ADDR_VECTOR
		lda #$00
		adc MON_ADDR_VECTOR+1
		sta MON_ADDR_VECTOR+1

		dex
		beq doMemEnd

		bra doMemLoopRow

doMemEnd
		rts
		.endproc

doJump .proc
		jsr skipToNextToken
		bcs doJumpBadAddress

		jsr getHexAddr
		bcc doJump2

doJumpBadAddress
		jsr MPutStr
		.null "bad or missing address",$0a

		bra doJumpEnd

doJump2
		lda HEX_ADDR_BUFFER
		sta MON_ADDR_VECTOR
		lda HEX_ADDR_BUFFER+1
		sta MON_ADDR_VECTOR+1

		jmp (MON_ADDR_VECTOR)

doJumpEnd
		rts
		.endproc

doContinue .proc
		clc
		lda BRK_PC_VECTOR
		adc #$02
		sta BRK_PC_VECTOR
		lda BRK_PC_VECTOR+1
		adc #$00
		sta BRK_PC_VECTOR+1

		ldx BRK_SAVE_SP
		txs
		pla						; SP on BRK was pointing after status and return addr
		pla						; so throw away the junk that is there
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
		.endproc

doBreakpoint .proc
		jsr skipToNextToken
		bcs doBreakpointToggle

		; process breakpoint arguments
		; 	b
		;		toggle breakpoint enable
		; 	b number
		;		remove breakpoint #number
		;			restore addr/addr+1 from breakpoint table
		;			remove from table
		;			error if breakpoint #number not in use
		; 	b number addr
		;		add breakpoint #number at addr
		; 			save bytes at addr/addr+1 to breakpoint table
		;			write BRK number at addr/addr+1
		;			error if breakpoint table entry already used
		; **** will need to modify doContinue
		; ****		lookup BRK_PC_VECTOR in table
		; ****		if not found, don't modify memory
		; ****		if found, restore memory and remove from table

		bra doBreakpointEnd

doBreakpointToggle
		lda #$01
		eor MON_BP_ENABLE
		sta MON_BP_ENABLE

		jsr MPutStr
		.null "breakpoints "

		lda MON_BP_ENABLE
		beq doBreakPointDisabled

		jsr MPutStr
		.null "enabled",$0a

		bra doBreakpointEnd

doBreakPointDisabled
		jsr MPutStr
		.null "disabled",$0a

doBreakpointEnd
		rts
		.endproc

doDisassemble .proc
		jsr skipToNextToken
		bcs doDisassembleHaveAddr

		jsr getHexAddr
		bcc doDisassembleGotAddr

doDisassembleBadAddress
		jsr MPutStr
		.null "bad address",$0a

		bra doDisassembleEnd

doDisassembleGotAddr
		lda HEX_ADDR_BUFFER
		sta MON_ADDR_VECTOR
		lda HEX_ADDR_BUFFER+1
		sta MON_ADDR_VECTOR+1

doDisassembleHaveAddr
		ldx #$14

doDisassembleLoop
		phx
		jsr Disassemble
		plx
		dex
		beq doDisassembleEnd

		clc
		adc MON_ADDR_VECTOR
		sta MON_ADDR_VECTOR
		lda #$00
		adc MON_ADDR_VECTOR+1
		sta MON_ADDR_VECTOR+1

		bra doDisassembleLoop

doDisassembleEnd
		rts

Disassemble .proc
		lda MON_ADDR_VECTOR+1		; output pc
		jsr MWriteHexByte
		lda MON_ADDR_VECTOR
		jsr MWriteHexByte
		jsr WriteSpace

		lda (MON_ADDR_VECTOR)		; save opcode
		sta MON_OPCODE

		sta MON_OPCODE_VECTOR		; calculate offset into opcode table
		stz MON_OPCODE_VECTOR+1

		clc
		asl MON_OPCODE_VECTOR
		rol MON_OPCODE_VECTOR+1		; opcode * 2

		clc
		lda MON_OPCODE
		adc MON_OPCODE_VECTOR
		sta MON_OPCODE_VECTOR
		lda #$0
		adc MON_OPCODE_VECTOR+1
		sta MON_OPCODE_VECTOR+1		; opcode * 3

		clc
		asl MON_OPCODE_VECTOR
		rol MON_OPCODE_VECTOR+1		; opcode * 6

		clc							; calculate absolute address in opcode tbl
		lda #<opcodes
		adc MON_OPCODE_VECTOR
		sta MON_OPCODE_VECTOR
		lda #>opcodes
		adc MON_OPCODE_VECTOR+1
		sta MON_OPCODE_VECTOR+1

		ldy #$05
		lda (MON_OPCODE_VECTOR),y	; instruction length
		tax

		ldy #$00
		lda (MON_ADDR_VECTOR),y		; opcode
		jsr MWriteHexByte
		jsr writeSpace

		dex
		beq bytesDone

		iny
		lda (MON_ADDR_VECTOR),y		; operand 1
		jsr MWriteHexByte
		jsr writeSpace

		dex
		beq bytesDone

		iny
		lda (MON_ADDR_VECTOR),y		; operand 2
		jsr MWriteHexByte
		jsr writeSpace

bytesDone
		ldy #$05					; add spaces to account for < 2 operands
		lda #$03
		sec
		sbc (MON_OPCODE_VECTOR),y
		tax
		beq spacesDone

spacesLoop
		jsr writeSpace
		jsr writeSpace
		jsr writeSpace
		dex
		bne spacesLoop

spacesDone
		ldx MON_OPCODE_VECTOR		; output instruction mnemonic
		ldy MON_OPCODE_VECTOR+1
		jsr KWRITESTR
		jsr writeSpace

		ldy #$04
		lda (MON_OPCODE_VECTOR),y	; get addressing mode
		jsr handleAddrMode			; output instruction operands
		iny
		lda (MON_OPCODE_VECTOR),y	; get instruction length
		jmp KWRITECRLF

handleAddrMode
		cmp #AM_ACCUMULATOR
		bne checkAbsolute

		; xxx
		jmp handleAddrModeDone

checkAbsolute
		cmp #AM_ABSOLUTE
		bne checkAbsoluteX

		; xxx $hilo
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$02
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		dey
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte

		ply
		jmp handleAddrModeDone

checkAbsoluteX
		cmp #AM_ABSOLUTE_X
		bne checkAbsoluteY

		; xxx $hilo,x
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$02
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		dey
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #','
		jsr KWRITETERM
		lda #'x'
		jsr KWRITETERM

		ply
		jmp handleAddrModeDone

checkAbsoluteY
		cmp #AM_ABSOLUTE_Y
		bne checkImmediate

		; xxx $hilo,y
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$02
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		dey
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #','
		jsr KWRITETERM
		lda #'y'
		jsr KWRITETERM

		ply
		jmp handleAddrModeDone

checkImmediate
		cmp #AM_IMMEDIATE
		bne checkImplied

		; xxx #$bb
		phy

		lda #'#'
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte

		ply
		jmp handleAddrModeDone

checkImplied
		cmp #AM_IMPLIED
		bne checkIndirect

		; xxx
		jmp handleAddrModeDone

checkIndirect
		cmp #AM_INDIRECT
		bne checkXIdxIndir

		; xxx ($hilo)
		phy

		lda #'('
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$02
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		dey
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #')'
		jsr KWRITETERM

		ply
		jmp handleAddrModeDone

checkXIdxIndir
		cmp #AM_X_IDX_INDIR
		bne checkIndirYIdx

		; xxx ($zp,x)
		phy

		lda #'('
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #','
		jsr KWRITETERM
		lda #'x'
		jsr KWRITETERM
		lda #')'
		jsr KWRITETERM

		ply
		jmp handleAddrModeDone

checkIndirYIdx
		cmp #AM_INDIR_Y_IDX
		bne checkRelative

		; xxx ($zp),y
		phy

		lda #'('
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #')'
		jsr KWRITETERM
		lda #','
		jsr KWRITETERM
		lda #'y'
		jsr KWRITETERM

		ply
		jmp handleAddrModeDone

checkRelative
		cmp #AM_RELATIVE
		bne checkZeroPage

		; xxx $ro => xxx $hilo
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		bpl forwardBranch

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
		bra adjustRelativeVector

forwardBranch
		clc						; forward relative branch
		adc MON_ADDR_VECTOR
		sta T1_VECTOR
		lda MON_ADDR_VECTOR+1
		adc #$0
		sta T1_VECTOR+1

adjustRelativeVector
		clc						; add 2 to get correct branch addr
		lda T1_VECTOR
		adc #$02
		sta T1_VECTOR
		lda T1_VECTOR+1
		adc #$00
		jsr MWriteHexByte
		lda T1_VECTOR
		jsr MWriteHexByte

didRelative
		ply
		jmp handleAddrModeDone

checkZeroPage
		cmp #AM_ZEROPAGE
		bne checkZeroPageX

		; xxx $zp
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte

		ply
		jmp handleAddrModeDone

checkZeroPageX
		cmp #AM_ZEROPAGE_X
		bne checkZeroPageY

		; xxx $zp,x
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #','
		jsr KWRITETERM
		lda #'x'
		jsr KWRITETERM

		ply
		bra handleAddrModeDone

checkZeroPageY
		cmp #AM_ZEROPAGE_Y
		bne checkAbsIdxInd

		; xxx $zp,y
		phy

		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #','
		jsr KWRITETERM
		lda #'y'
		jsr KWRITETERM

		ply
		bra handleAddrModeDone

checkAbsIdxInd
		cmp #AM_ABS_IDX_IND
		bne checkZeroPageInd

		; xxx ($hilo),x
		phy

		lda #'('
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$02
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		dey
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #')'
		jsr KWRITETERM
		lda #','
		jsr KWRITETERM
		lda #'x'
		jsr KWRITETERM

		ply
		bra handleAddrModeDone

checkZeroPageInd
		cmp #AM_ZEROPAGE_IND
		bne unknownAddrMode

		; xxx ($zp)
		phy

		lda #'('
		jsr KWRITETERM
		lda #'$'
		jsr KWRITETERM
		ldy #$01
		lda (MON_ADDR_VECTOR),y
		jsr MWriteHexByte
		lda #')'
		jsr KWRITETERM

		ply
		bra handleAddrModeDone

unknownAddrMode
		jsr MPutStr
		.null "addr mode "
		jsr MWriteHexByte

handleAddrModeDone
		rts

; Addressing modes
AM_ACCUMULATOR		= 1
AM_ABSOLUTE			= 2
AM_ABSOLUTE_X		= 3
AM_ABSOLUTE_Y		= 4
AM_IMMEDIATE		= 5
AM_IMPLIED			= 6
AM_INDIRECT			= 7
AM_X_IDX_INDIR		= 8
AM_INDIR_Y_IDX		= 9
AM_RELATIVE			= 10
AM_ZEROPAGE			= 11
AM_ZEROPAGE_X		= 12
AM_ZEROPAGE_Y		= 13
AM_ABS_IDX_IND 		= 14
AM_ZEROPAGE_IND		= 15

opcodes
		.null "brk"
		.byte AM_IMMEDIATE,2
		.null "ora"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_ZEROPAGE,2
		.null "asl"
		.byte AM_ZEROPAGE,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "php"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_IMMEDIATE,2
		.null "asl"
		.byte AM_ACCUMULATOR,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_ABSOLUTE,3
		.null "asl"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bpl"
		.byte AM_RELATIVE,2
		.null "ora"
		.byte AM_INDIR_Y_IDX,2
		.null "ora"
		.byte AM_ZEROPAGE_IND,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_ZEROPAGE_X,2
		.null "asl"
		.byte AM_ZEROPAGE_X,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "clc"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_ABSOLUTE_Y,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ora"
		.byte AM_ABSOLUTE_X,3
		.null "asl"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "jsr"
		.byte AM_ABSOLUTE,3
		.null "and"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bit"
		.byte AM_ZEROPAGE,2
		.null "and"
		.byte AM_ZEROPAGE,2
		.null "rol"
		.byte AM_ZEROPAGE,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "plp"
		.byte AM_IMPLIED,1
		.null "and"
		.byte AM_IMMEDIATE,2
		.null "rol"
		.byte AM_ACCUMULATOR,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bit"
		.byte AM_ABSOLUTE,3
		.null "abs"
		.byte AM_ABSOLUTE,3
		.null "rol"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bmi"
		.byte AM_RELATIVE,2
		.null "and"
		.byte AM_INDIR_Y_IDX,2
		.null "and"
		.byte AM_ZEROPAGE_IND,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bit"
		.byte AM_ZEROPAGE_X,2
		.null "and"
		.byte AM_ZEROPAGE_X,2
		.null "rol"
		.byte AM_ZEROPAGE_X,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "sec"
		.byte AM_IMPLIED,1
		.null "and"
		.byte AM_ABSOLUTE_Y,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bit"
		.byte AM_ABSOLUTE_X,3
		.null "and"
		.byte AM_ABSOLUTE_X,3
		.null "rol"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "rti"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_ZEROPAGE,2
		.null "lsr"
		.byte AM_ZEROPAGE,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "pha"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_IMMEDIATE,2
		.null "lsr"
		.byte AM_ACCUMULATOR,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "jmp"
		.byte AM_ABSOLUTE,3
		.null "eor"
		.byte AM_ABSOLUTE,3
		.null "lsr"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bvc"
		.byte AM_RELATIVE,2
		.null "eor"
		.byte AM_INDIR_Y_IDX,2
		.null "eor"
		.byte AM_ZEROPAGE_IND,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_ZEROPAGE_X,2
		.null "lsr"
		.byte AM_ZEROPAGE_X,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "cli"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_ABSOLUTE_Y,3
		.null "phy"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "eor"
		.byte AM_ABSOLUTE_X,3
		.null "lsr"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "rts"
		.byte AM_IMPLIED,1
		.null "adc"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "stz"
		.byte AM_ZEROPAGE,2
		.null "adc"
		.byte AM_ZEROPAGE,2
		.null "ror"
		.byte AM_ZEROPAGE,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "pla"
		.byte AM_IMPLIED,1
		.null "adc"
		.byte AM_IMMEDIATE,2
		.null "ror"
		.byte AM_ACCUMULATOR,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "jmp"
		.byte AM_INDIRECT,3
		.null "adc"
		.byte AM_ABSOLUTE,3
		.null "ror"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bvs"
		.byte AM_RELATIVE,2
		.null "adc"
		.byte AM_INDIR_Y_IDX,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "stz"
		.byte AM_ZEROPAGE_X,2
		.null "adc"
		.byte AM_ZEROPAGE_X,2
		.null "ror"
		.byte AM_ZEROPAGE_X,2
		.null "rmb"
		.byte AM_ZEROPAGE,2
		.null "sei"
		.byte AM_IMPLIED,1
		.null "adc"
		.byte AM_ABSOLUTE_Y,3
		.null "ply"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "jmp"
		.byte AM_ABS_IDX_IND,3
		.null "adc"
		.byte AM_ABSOLUTE_X,3
		.null "ror"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bra"
		.byte AM_RELATIVE,2
		.null "sta"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "sty"
		.byte AM_ZEROPAGE,2
		.null "sta"
		.byte AM_ZEROPAGE,2
		.null "stx"
		.byte AM_ZEROPAGE,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "dey"
		.byte AM_IMPLIED,1
		.null "bit"
		.byte AM_IMMEDIATE,2
		.null "txa"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "sty"
		.byte AM_ABSOLUTE,3
		.null "sta"
		.byte AM_ABSOLUTE,3
		.null "stx"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bcc"
		.byte AM_RELATIVE,2
		.null "sta"
		.byte AM_INDIR_Y_IDX,2
		.null "sta"
		.byte AM_ZEROPAGE_IND,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "sty"
		.byte AM_ZEROPAGE_X,2
		.null "sta"
		.byte AM_ZEROPAGE_X,2
		.null "stx"
		.byte AM_ZEROPAGE_Y,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "tya"
		.byte AM_IMPLIED,1
		.null "sta"
		.byte AM_ABSOLUTE_Y,3
		.null "txs"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "stz"
		.byte AM_ABSOLUTE,3
		.null "sta"
		.byte AM_ABSOLUTE_X,3
		.null "stz"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ldy"
		.byte AM_IMMEDIATE,2
		.null "lda"
		.byte AM_X_IDX_INDIR,2
		.null "ldx"
		.byte AM_IMMEDIATE,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ldy"
		.byte AM_ZEROPAGE,2
		.null "lda"
		.byte AM_ZEROPAGE,2
		.null "ldx"
		.byte AM_ZEROPAGE,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "tay"
		.byte AM_IMPLIED,1
		.null "lda"
		.byte AM_IMMEDIATE,2
		.null "tax"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ldy"
		.byte AM_ABSOLUTE,3
		.null "lda"
		.byte AM_ABSOLUTE,3
		.null "ldx"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bcs"
		.byte AM_RELATIVE,2
		.null "lda"
		.byte AM_INDIR_Y_IDX,2
		.null "lda"
		.byte AM_ZEROPAGE_IND,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ldy"
		.byte AM_ZEROPAGE_X,2
		.null "lda"
		.byte AM_ZEROPAGE_X,2
		.null "ldx"
		.byte AM_ZEROPAGE_Y,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "clv"
		.byte AM_IMPLIED,1
		.null "lda"
		.byte AM_ABSOLUTE_Y,3
		.null "tsx"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "ldy"
		.byte AM_ABSOLUTE_X,3
		.null "lda"
		.byte AM_ABSOLUTE_X,3
		.null "ldx"
		.byte AM_ABSOLUTE_Y,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpy"
		.byte AM_IMMEDIATE,2
		.null "cmp"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpy"
		.byte AM_ZEROPAGE,2
		.null "cmp"
		.byte AM_ZEROPAGE,2
		.null "dec"
		.byte AM_ZEROPAGE,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "iny"
		.byte AM_IMPLIED,1
		.null "cmp"
		.byte AM_IMMEDIATE,2
		.null "dex"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpy"
		.byte AM_ABSOLUTE,3
		.null "cmp"
		.byte AM_ABSOLUTE,3
		.null "dec"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "bne"
		.byte AM_RELATIVE,2
		.null "cmp"
		.byte AM_INDIR_Y_IDX,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cmp"
		.byte AM_ZEROPAGE_X,2
		.null "dec"
		.byte AM_ZEROPAGE_X,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "cld"
		.byte AM_IMPLIED,1
		.null "cmp"
		.byte AM_ABSOLUTE_Y,3
		.null "phx"
		.byte AM_IMPLIED,1
		.null "STP"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cmp"
		.byte AM_ABSOLUTE_X,3
		.null "dec"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpx"
		.byte AM_IMMEDIATE,2
		.null "sbc"
		.byte AM_X_IDX_INDIR,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpx"
		.byte AM_ZEROPAGE,2
		.null "sbc"
		.byte AM_ZEROPAGE,2
		.null "inc"
		.byte AM_ZEROPAGE,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "inx"
		.byte AM_IMPLIED,1
		.null "sbc"
		.byte AM_IMMEDIATE,2
		.null "nop"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "cpx"
		.byte AM_ABSOLUTE,3
		.null "sbc"
		.byte AM_ABSOLUTE,3
		.null "inc"
		.byte AM_ABSOLUTE,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "beq"
		.byte AM_RELATIVE,2
		.null "sbc"
		.byte AM_INDIR_Y_IDX,2
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "sbc"
		.byte AM_ZEROPAGE_X,2
		.null "inc"
		.byte AM_ZEROPAGE_X,2
		.null "smb"
		.byte AM_ZEROPAGE,2
		.null "sed"
		.byte AM_IMPLIED,1
		.null "sbc"
		.byte AM_ABSOLUTE_Y,3
		.null "plx"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "BAD"
		.byte AM_IMPLIED,1
		.null "sbc"
		.byte AM_ABSOLUTE_X,3
		.null "inc"
		.byte AM_ABSOLUTE_X,3
		.null "BAD"
		.byte AM_IMPLIED,1
		.endproc
		.endproc

doLoad .proc
		jsr skipToNextToken
		bcc getFileName

		jmp doHelp

getFileName
		ldy #$000

getFileNameLoop
		lda INPUT_BUFFER,x
		sta FILEIO_FILENAME,y
		beq gotFileName
		inx
		iny
		bra getFileNameLoop

gotFileName
		jsr MPutStr
		.null "loading file "
		ldx #<FILEIO_FILENAME
		ldy #>FILEIO_FILENAME
		jsr KWRITESTR
		jsr KWRITECRLF

		jsr fileIOOpen
		beq loadFileAddr

		jsr MPutStr
		.null "Could not open file",$0a
		jmp loadFileDone

loadFileAddr
		jsr fileIORead
		bne loadFileError

		lda FILEIO_CDATA_LO
		sta T1_VECTOR
		jsr fileIORead
		bne loadFileError

		lda FILEIO_CDATA_LO
		sta T1_VECTOR+1

		jsr MPutStr
		.null "> "
		lda T1_VECTOR+1
		jsr MWriteHexByte
		lda T1_VECTOR
		jsr MWriteHexByte
		jsr KWRITECRLF

		bra loadFileData

loadFileError
		jsr MPutStr
		.null "Could not load file (a)",$0a
		jsr fileIOClose

		jmp loadFileDone

loadFileData
		jsr fileIORead
		bne loadFileDataDone
		lda FILEIO_CDATA_LO
		sta (T1_VECTOR)
		clc
		lda T1_VECTOR
		adc #$01
		sta T1_VECTOR
		lda T1_VECTOR+1
		adc #$00
		sta T1_VECTOR+1
		bra loadFileData

loadFileDataDone
		lda FILEIO_CSTATUS
		cmp #FILEIO_STATUS_EOF
		beq closeFile

		jsr MPutStr
		.null "Could not load file (b)",$0a

closeFile
		jsr fileIOClose

loadFileDone
		rts
		.endproc

fileIOWait .proc
		lda FILEIO_CREADY
		bne fileIOWait
		rts
		.endproc

fileIOOpen .proc
		lda #FILEIO_MODE_READ
		sta FILEIO_CMODE
		lda #FILEIO_CMD_OPEN
		sta FILEIO_CCMD
		lda #$01
		sta FILEIO_CREADY
		jsr fileIOWait
		lda FILEIO_CSTATUS
		rts
		.endproc

fileIOClose .proc			; assumes fd in already stored at FILEIO_CFD
		lda #FILEIO_CMD_CLOSE
		sta FILEIO_CCMD
		lda #$01
		sta FILEIO_CREADY
		jsr fileIOWait
		lda FILEIO_CSTATUS
		rts
		.endproc

fileIORead .proc			; assumes fd is already stored at FILEIO_CFD
		lda #FILEIO_CMD_READ
		sta FILEIO_CCMD
		lda #$01
		sta FILEIO_CREADY
		jsr fileIOWait
		lda FILEIO_CSTATUS
		rts
		.endproc

doTest .proc
		stz $80
		lda #1
		pha
		jsr doTest2
		pla
		rts
doTest2
		tsx
		inx
		inx
		inx
		lda $0100,x
		sta $80

		rts
		.endproc

doHelp .proc
		jsr MPutStr
		.text "Commands:",$0a
		.text "  q                   quit",$0a
		.text "  w addr bb [bb..]    write bytes at addr",$0a
		.text "  m [addr]            dump memory at [addr] or most recent addr",$0a
		.text "  j addr              jump to address",$0a
		.text "  c                   continue (from breakpoint)",$0a
		.text "  b                   toggle breakpoints",$0a
		.text "  d [addr]            disassemble 20 instructions at [addr] or most recent addr",$0a
		.text "  l filename          load binary file",$0a
		.text "                      assumes load address in first 2 bytes of file",$0a
		.null "  h                   display this help text",$0a
		rts
		.endproc
		.endproc

MHexToDigit .proc
		cmp #$47
		blt MHexToDigit1

		and #$DF

MHexToDigit1
		sec
		sbc #$30
		cmp #$0a
		blt MHexToDigitDone
		sec
		sbc #$07

MHexToDigitDone
		rts
		.endproc

getHexByte .proc
		cpx #INPUT_BUFFER_LEN
		beq getHexByteBad

		lda INPUT_BUFFER,x
		beq getHexByteBad

		jsr MHexToDigit
		asl
		asl
		asl
		asl
		sta HEX_BYTE_BUFFER

		inx
		cpx #INPUT_BUFFER_LEN
		beq getHexByteBad

		lda INPUT_BUFFER,x
		beq getHexByteBad

		jsr MHexToDigit
		ora HEX_BYTE_BUFFER
		sta HEX_BYTE_BUFFER

		clc
		bra getHexByteDone

getHexByteBad
		sec

getHexByteDone
		rts
		.endproc

getHexAddr .proc
		jsr getHexByte
		bcs getHexAddrDone

		lda HEX_BYTE_BUFFER
		sta HEX_ADDR_BUFFER+1

		inx
		cpx #INPUT_BUFFER_LEN
		bne getHexAddr1

		sec
		bra getHexAddrDone

getHexAddr1
		jsr getHexByte
		bcs getHexAddrDone

		lda HEX_BYTE_BUFFER
		sta HEX_ADDR_BUFFER

getHexAddrDone
		rts
		.endproc

MByteToHexDigit .proc
		cmp #$0a
		blt MByteToHexDigit1

		clc
		adc #$07

MByteToHexDigit1
		clc
		adc #$30

		rts
		.endproc

MWriteHexByte .proc
		pha
		pha

		lsr
		lsr
		lsr
		lsr

		jsr MByteToHexDigit
		jsr KWRITETERM

		pla
		and #$0f

		jsr MByteToHexDigit
		jsr KWRITETERM

		pla
		rts
		.endproc

MWriteBinaryByte .proc
		pha
		phx

		clc
		ldx #$08

writeBinaryDigit1
		asl
		pha

		bcc writeBinaryDigit2

		lda #'1'
		bra writeBinaryDigit3

writeBinaryDigit2
		lda #'0'

writeBinaryDigit3
		jsr KWRITETERM

		pla

		dex
		bne writeBinaryDigit1

		plx
		pla

		rts
		.endproc

skipToNextToken .proc
		jsr skipToSpace

		cpx #INPUT_BUFFER_LEN
		beq noToken

		lda INPUT_BUFFER,x
		beq noToken

		jsr skipSpaces

		cpx #INPUT_BUFFER_LEN
		beq noToken

		lda INPUT_BUFFER,x
		beq noToken

		clc

		bra skipToNextTokenDone

noToken
		sec

skipToNextTokenDone
		rts
		.endproc

skipToSpace .proc
		cpx #INPUT_BUFFER_LEN
		beq skipToSpaceDone

		lda INPUT_BUFFER,x
		beq skipToSpaceDone

		cmp #$20
		beq skipToSpaceDone

		inx
		bra skipToSpace

skipToSpaceDone
		rts
		.endproc

skipSpaces .proc
		cpx #INPUT_BUFFER_LEN
		beq skipSpacesDone

		lda INPUT_BUFFER,x
		beq skipSpacesDone

		cmp #$20
		bne skipSpacesDone

		inx
		bra skipSpaces

skipSpacesDone
		rts
		.endproc

writeSpace .proc
		pha

		lda #' '
		jsr KWRITETERM

		pla
		rts
		.endproc

;*****************************************************************************
; Function:		MPutStr
; Description:	Writes z-string imbeded after the jsr instruction to the
;				terminal
; Input:		nothing
; Changes:		nothing
; Returns:		nothing
;*****************************************************************************
MPutStr .proc
		stx XSAVE2
		sty YSAVE2
		sta ASAVE2

		pla			; get return address into zero-page
		sta WIMT_VECTOR
		pla
		sta WIMT_VECTOR+1

		ldy #$00
nextChar
		inc WIMT_VECTOR
		bne writeChar
		inc WIMT_VECTOR+1

writeChar
		lda (WIMT_VECTOR),y
		beq done
		jsr KWRITETERM

		bra nextChar

done
		lda WIMT_VECTOR+1
		pha
		lda WIMT_VECTOR
		pha

		ldx XSAVE2
		ldy YSAVE2
		lda ASAVE2

		rts
		.endproc
		.endblock

;*****************************************************************************
; Function:		KMONINIT
; Description:	Initializes monitor data
; Input:		nothing
; Changes:		a
; Returns:		nothing
;*****************************************************************************
KMONINIT .block
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
		.endblock
