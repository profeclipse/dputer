.ifdef DPUTER
        .segment "LOADADDR"
        .word *+2
.endif

		.segment "HEADER"
.ifdef KBD
        jmp     LE68C
        .byte   $00,$13,$56
.endif
.ifdef AIM65
        jmp     COLD_START
        jmp     RESTART
        .word   AYINT,GIVAYF
.endif
.ifdef SYM1
        jmp     PR_WRITTEN_BY
.endif
.ifdef DPUTER
        jmp     PR_WRITTEN_BY
.endif
