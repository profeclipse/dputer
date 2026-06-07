ISCNTC:
    jsr TERMREAD
    bcc @not_cntc
    cmp #3
    bne @not_cntc
    bra @is_cntc

@not_cntc:
    rts

@is_cntc:
    ; fall through
