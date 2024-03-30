;*****************************************************************************
; kernel.s - kernel for dputer
;*****************************************************************************
; Memory Map
;   $0000 - $00CF       User Zero Page
;   $00D0 - $00FF       Reserved Zero Page
;   $0100 - $01FF       Stack
;   $0200 - $02FF       Memory Mapped I/O
;       $0200 - $020F       Terminal I/O
;       $0210 - $02FF       File I/O
;   $0300 - $03FF       Kernel Keyboard Buffer
;   $0400 - $04FF       Kernel Input Buffer
;   $0500 - $DFFF       User RAM
;   $E000 - $FFF8       Kernel
;   $FFFA - $FFFB       NMI Vector
;   $FFFC - $FFFD       Reset Vector
;   $FFFE - $FFFF       IRQ Vector
;*****************************************************************************

    .setcpu "65C02"
    .feature org_per_seg
    .feature string_escapes
    .debuginfo 

    .include "kernel.inc"

    .include "zeropage.s"
    .include "loadaddr.s"
    .include "bios.s"
    .include "termio.s"
    .include "fileio.s"
    .include "monitor.s"
    .include "jumptable.s"
    .include "vectors.s"

