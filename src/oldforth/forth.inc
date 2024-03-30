;*****************************************************************************
; forth.inc
;*****************************************************************************
VERSIONNUM				= 010100

DSTACK					= TERMIO_BASE-$0410	; data stack   - D7F0-DBEF
RSTACK					= DSTACK-$0400		; return stack - D3F0-D7EF

DSTACK_TOP				= DSTACK+$03FE
RSTACK_TOP				= RSTACK+$03FE

SP						= USER_ZP	; data stack pointer
RP						= SP+2		; return stack pointer
IP						= RP+2		; instruction pointer
CFA						= IP+2		; current cfa
RCFA					= CFA+2		; "real" cfa
UP						= RCFA+2	; user pointer (not used)
GP0						= UP+2		; general purpose register 0
GP1						= GP0+2		; general purpose register 1
GP2						= GP1+2		; general purpose register 1
GP3						= GP2+2		; general purpose register 1
GP4						= GP3+2		; general purpose register 1
GP5						= GP4+2		; general purpose register 1
GP6						= GP5+2		; general purpose register 1
GP7						= GP6+2		; general purpose register 1
GP8						= GP7+2		; general purpose register 1
GP9						= GP8+2		; general purpose register 1

; ---------------------------------------------------------------------------
; Some manifest constants
; ---------------------------------------------------------------------------

MAXBUFSIZE				= 255

NUMVOCSDEF				= 16

; ---------------------------------------------------------------------------
; Abort codes
; ---------------------------------------------------------------------------

; ANS defined
THROW_ABORT				= $FFFF
THROW_ABORTQ			= $FFFE
THROW_STACKUNDER		= $FFFC
THROW_UNDEFINED			= $FFF3
THROW_COMPONLY			= $FFF4
THROW_NAMEREQD			= $FFF0
THROW_MISMATCH			= $FFEA
THROW_FILENOTFOUND		= $FFDA

; System extended
THROW_NOTDEFER			= $FEFC
THROW_NOTVALUE			= $FEFB
THROW_OUTOFMEM			= $FEFA
THROW_FILECREATEFAIL	= $FEF9
THROW_FILEREADFAIL		= $FEF8
THROW_FILEWRITEFAIL		= $FEF7
THROW_EXECONLY			= $FEF6
THROW_LOCALSTWICE		= $FEF5
THROW_TOOMANYLOCALS		= $FEF4
THROW_LOCALSNOCLOSE		= $FEF3
THROW_STACKCHG			= $FEF2
THROW_ARGSNODASHDASH	= $FEF1
THROW_NOTLOCAL			= $FEF0
THROW_FILECLOSEFAIL		= $FFEF
