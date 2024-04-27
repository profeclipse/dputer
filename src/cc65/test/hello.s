    .setcpu "65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo

    .segment "LOADADDR"

    .word LOADADDR

    .segment "CODE"

LOADADDR = *

ENTRY:
    stp
