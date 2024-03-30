;*****************************************************************************
; forth.inc
;*****************************************************************************
VERSIONNUM              = 010100

DSTACK                  = $E000-$0400       ; data stack   - DC00-DFFF
RSTACK                  = DSTACK-$0400      ; return stack - D800-DBFF

DSTACK_TOP              = DSTACK+$03FE
RSTACK_TOP              = RSTACK+$03FE

; ---------------------------------------------------------------------------
; Some manifest constants
; ---------------------------------------------------------------------------

MAXBUFSIZE              = 255

NUMVOCSDEF              = 16

; ---------------------------------------------------------------------------
; Abort codes
; ---------------------------------------------------------------------------

; ANS defined
THROW_ABORT             = $FFFF
THROW_ABORTQ            = $FFFE
THROW_STACKUNDER        = $FFFC
THROW_UNDEFINED         = $FFF3
THROW_COMPONLY          = $FFF4
THROW_NAMEREQD          = $FFF0
THROW_MISMATCH          = $FFEA
THROW_FILENOTFOUND      = $FFDA

; System extended
THROW_NOTDEFER          = $FEFC
THROW_NOTVALUE          = $FEFB
THROW_OUTOFMEM          = $FEFA
THROW_FILECREATEFAIL    = $FEF9
THROW_FILEREADFAIL      = $FEF8
THROW_FILEWRITEFAIL     = $FEF7
THROW_EXECONLY          = $FEF6
THROW_LOCALSTWICE       = $FEF5
THROW_TOOMANYLOCALS     = $FEF4
THROW_LOCALSNOCLOSE     = $FEF3
THROW_STACKCHG          = $FEF2
THROW_ARGSNODASHDASH    = $FEF1
THROW_NOTLOCAL          = $FEF0
THROW_FILECLOSEFAIL     = $FFEF
