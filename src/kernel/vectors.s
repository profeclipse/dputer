    .setcpu "65C02"

    .segment "VECTORS"

    .word NMI_HANDLER
    .word KRESET
    .word IRQ_HANDLER
