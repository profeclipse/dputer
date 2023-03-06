;*****************************************************************************
; kernel.inc
;*****************************************************************************
; Memory Map
; 	0000 - 00CF		User Zero Page
;	00D0 - 00FF		Reserved Zero Page
;	0100 - 01FF		Stack
;	0200 - DAFF		Free RAM
;	DB00 - DCFF		Memory Mapped I/O
;	  DB00 - DB06		Terminal I/O
;	  DB10 - DCFF		File I/O
;	DD00 - DDFF		Keyboard Buffer
;	DE00 - DEFF		Kernel Input Buffer
;	E000 - FFF8		Kernel
;	FFFA - FFFB		NMI Vector
;	FFFC - FFFD		Reset Vector
;	FFFE - FFFF		IRQ Vector
;*****************************************************************************

; zero page vectors
STROUT_VECTOR		= $D0		; 2 bytes
STRIN_VECTOR		= $D2		; 2 bytes
T1_VECTOR			= $D4		; 2 bytes
ASAVE				= $D6
XSAVE				= $D7
YSAVE				= $D8
PSAVE				= $D9
ASAVE2				= $DA
XSAVE2				= $DB
YSAVE2				= $DC
WIMT_VECTOR			= $DD		; 2 bytes
MON_CMD_VECTOR		= $DF		; 2 bytes
MON_ADDR_VECTOR		= $E1		; 2 bytes
MON_OPCODE_VECTOR	= $E3		; 2 bytes
BRK_SAVE_A			= $E5
BRK_SAVE_X			= $E6
BRK_SAVE_Y			= $E7
BRK_SAVE_SP			= $E8
BRK_PC_VECTOR		= $E9		; 2 bytes
BRK_STATUS_REG		= $EB
MON_FROM_BRK		= $EC
MON_BP_ENABLE		= $ED
KBD_WPTR			= $EE
KBD_RPTR			= $EF
MON_OPCODE			= $F0
IRQ_SAVE_A			= $F1
IRQ_SAVE_X			= $F2
IRQ_SAVE_Y			= $F3
IRQ_SAVE_SP			= $F4
HEX_ADDR_BUFFER		= $F5		; 2 byte hex address buffer
HEX_BYTE_BUFFER		= $F7		; 1 byte hex byte buffer
MON_COLD_START		= $F8		; 1 byte monitor cold start flag
USER_ZP				= $00		; start of user zp space

; general purpose data
KBD_BUFFER			= $DD00		; 255 byte keyboard buffer
INPUT_BUFFER		= $DE00		; 255 byte input buffer
INPUT_BUFFER_LEN	= $FF		; max length of input string
