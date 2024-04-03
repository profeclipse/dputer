.segment "CODE"

ISCNTC:
    jsr MONRDKEY
    bcc @not_cntc
    cmp #$03
    bne @not_cntc
    bra @cont
@not_cntc:
    rts
@cont:
    ; fall through
