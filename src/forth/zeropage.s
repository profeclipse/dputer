    .segment "ZEROPAGE"

SP:     .res 2      ; data stack pointer
RP:     .res 2      ; return stack pointer
IP:     .res 2      ; instruction pointer
CFA:    .res 2      ; current cfa
RCFA:   .res 2      ; "real" cfa
UP:     .res 2	    ; user pointer (not used)
GP0:    .res 2      ; general purpose register 0
GP1:    .res 2      ; general purpose register 1
GP2:    .res 2      ; general purpose register 2
GP3:    .res 2      ; general purpose register 3
GP4:    .res 2      ; general purpose register 4
GP5:    .res 2      ; general purpose register 5
GP6:    .res 2      ; general purpose register 6
GP7:    .res 2      ; general purpose register 7
GP8:    .res 2      ; general purpose register 8
GP9:    .res 2      ; general purpose register 9
GPTEMP: .res 2      ; general purpose temp register (only use in a single code word)

