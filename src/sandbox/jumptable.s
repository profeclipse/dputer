    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .segment "JUMPTABLE"

LCDoutch:           jmp lcd_outch
LCDprint:           jmp lcd_print
LCDclear:           jmp lcd_clear
LCDinstruction:     jmp lcd_instruction
LCDwait:            jmp lcd_wait
LCDbusy:            jmp lcd_busy
ACIAread:           jmp acia_read
ACIAwrite:          jmp acia_write
TERMread:           jmp term_read
TERMget_char:       jmp term_get_char
TERMget_string:     jmp term_get_string
TERMunread:         jmp term_unread
TERMhaschar:        jmp term_haschar
TERMwrite:          jmp term_write
TERMwrite_string:   jmp term_write_string
TERMwrite_crlf:     jmp term_write_crlf
TIMERsleep:         jmp timer_sleep
MONITOR:            jmp monitor
