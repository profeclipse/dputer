    .setcpu "W65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .segment "VECTORS"
    .word 0
    .word bios
    .word biosIRQ

