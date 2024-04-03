.setcpu "65C02"

.include "../kernel/kernel.inc"

; configuration
CONFIG_2A               := 1

CONFIG_SCRTCH_ORDER     := 2

; zero page
ZP_START0               = $00
ZP_START1               = $02
ZP_START2               = $0C
ZP_START3               = $62
ZP_START4               = $6D

USR                     := $03

SPACE_FOR_GOSUB         := $44
STACK_TOP		        := $FD
WIDTH			        := 80
WIDTH2			        := 70

; memory layout
RAMSTART2	            := $0500

CHROUT                  := $FF83
CHRIN                   := $FF89
MONCOUT                 := CHROUT
MONRDKEY                := CHRIN
