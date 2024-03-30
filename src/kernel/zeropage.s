    .setcpu "65C02"

    .segment "ZEROPAGE"

    .org RESERVED_ZP

STROUT_VECTOR:      .res 2
STRIN_VECTOR:       .res 2
FNAMEIN_VECTOR:     .res 2
T1_VECTOR:          .res 2
ASAVE:              .res 1
XSAVE:              .res 1
YSAVE:              .res 1
PSAVE:              .res 1
ASAVE2:             .res 1
XSAVE2:             .res 1
YSAVE2:             .res 1
WIMT_VECTOR:        .res 2
MON_CMD_VECTOR:     .res 2
MON_ADDR_VECTOR:    .res 2
MON_OPCODE_VECTOR:  .res 2
BRK_SAVE_A:         .res 1
BRK_SAVE_X:         .res 1
BRK_SAVE_Y:         .res 1
BRK_SAVE_SP:        .res 1
BRK_PC_VECTOR:      .res 2
BRK_STATUS_REG:     .res 1
MON_FROM_BRK:       .res 1
MON_BP_ENABLE:      .res 1
KBD_WPTR:           .res 1
KBD_RPTR:           .res 1
MON_OPCODE:         .res 1
IRQ_SAVE_A:         .res 1
IRQ_SAVE_X:         .res 1
IRQ_SAVE_Y:         .res 1
IRQ_SAVE_SP:        .res 1
HEX_ADDR_BUFFER:    .res 2
HEX_BYTE_BUFFER:    .res 1
MON_COLD_START:     .res 1
MON_FD:             .res 1
