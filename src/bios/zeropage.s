    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .segment "ZEROPAGE"

    .exportzp TIMER_TICKS, SLEEP_TICKS, SLEEP_TIME, MON_JUMP_VECTOR
    .exportzp MON_HEX_ADDR_BUFFER, MON_HEX_BYTE_BUFFER, MON_ADDR_VECTOR
    .exportzp STRIN_VECTOR, STROUT_VECTOR, KBD_WPTR, KBD_RPTR
    
    .org $100 - 19

TIMER_TICKS:         .res 4
SLEEP_TICKS:         .res 1
SLEEP_TIME:          .res 1
MON_JUMP_VECTOR:     .res 2
MON_HEX_ADDR_BUFFER: .res 2
MON_HEX_BYTE_BUFFER: .res 1
MON_ADDR_VECTOR:     .res 2
STRIN_VECTOR:        .res 2
STROUT_VECTOR:       .res 2
KBD_WPTR:            .res 1
KBD_RPTR:            .res 1
