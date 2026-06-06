    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .segment "JUMPTABLE"

    .import lcd_outch, lcd_print, lcd_clear, lcd_instruction
    .import acia_read, acia_write
    .import term_read, term_get_char, term_get_string, term_unread
    .import term_haschar, term_write, term_write_string, term_write_crlf
    .import timer_sleep
    .import monitor
;    .export LCDoutch, LCDprint, LCDclear, LCDinstruction
;    .export TERMread, TERMget_char, TERMget_string, TERMunread
;    .export TERMhaschar, TERMwrite, TERMwrite_string, TERMwrite_crlf

LCDOUTCH:           jmp lcd_outch
LCDPRINT:           jmp lcd_print
LCDCLEAR:           jmp lcd_clear
LCDINST:            jmp lcd_instruction
ACIAREAD:           jmp acia_read
ACIAWRITE:          jmp acia_write
TERMREAD:           jmp term_read
TERMGETCH:          jmp term_get_char
TERMGETSTR:         jmp term_get_string
TERMUNGET:          jmp term_unread
TERMHASCH:          jmp term_haschar
TERMWRITE:          jmp term_write
TERMWRITESTR:       jmp term_write_string
TERMWRITECRLF:      jmp term_write_crlf
TIMERSLEEP:         jmp timer_sleep
MONITOR:            jmp monitor
