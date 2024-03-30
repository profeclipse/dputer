; configuration
CONFIG_2C := 1

CONFIG_NULL := 1
CONFIG_MONCOUT_DESTROYS_Y := 1
CONFIG_PEEK_SAVE_LINNUM := 1
CONFIG_PRINT_CR := 1 ; print CR when line end reached
CONFIG_ROR_WORKAROUND := 1
CONFIG_SAFE_NAMENOTFOUND := 1
CONFIG_SCRTCH_ORDER := 1

; zero page
ZP_START1 = $17
ZP_START2 = $2F
ZP_START3 = $24
ZP_START4 = $85

;extra ZP variables
USR				:= $0021
TXPSV           := $00BA

; constants
STACK_TOP		:= $FE
SPACE_FOR_GOSUB := $3E
NULL_MAX		:= $F0
WIDTH			:= 80
WIDTH2			:= 56
; memory layout
RAMSTART2 := $0400

; monitor functions
MONRDKEY        := $E210
MONRDKEY2       := $E213
MONCOUT         := $E216
LF000 := $F000
LF003 := $F003
LF006 := $F006
LF009 := $F009
LF00C := $F00C
LF00F := $F00F
LF018 := $F018
LF01B := $F01B
LF01E := $F01E
LF021 := $F021
LFDFA := $FDFA
LFE73 := $FE73
LFE75 := $FE75
