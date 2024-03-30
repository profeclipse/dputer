    .setcpu "65C02"

    .segment "KERNEL"


; ****************************************************************************
; Function:     KFILEINIT
; Description:  Initializes file I/O
; Input:        nothing
; Changes:      nothing
; Returns:      nothing
; ****************************************************************************
KFILEINIT:
    stz FILEIO::READY
    stz FILEIO::CMD
    stz FILEIO::FD
    stz FILEIO::MODE
    stz FILEIO::DATA_LO
    stz FILEIO::DATA_HI
    stz FILEIO::DATA_LO2
    stz FILEIO::DATA_HI2
    stz FILEIO::STATUS
    stz FILEIO::FILENAME
    rts


; ----------------------------------------------------------------------------
; Wait for CREADY clear
; ----------------------------------------------------------------------------
kWaitForFileCmd:
    pha

@checkReady:
    lda FILEIO::READY
    bne @checkReady

    pla
    rts

; ----------------------------------------------------------------------------
; Do FileIO command
; ----------------------------------------------------------------------------
kDoFileCommand:
    lda #$ff
    sta FILEIO::READY

    bra kWaitForFileCmd


; ****************************************************************************
; Function:     KFILEOPEN
; Description:  Open a file
; Input:        a - open mode
;               x - filename (lo)
;               y - filename (hi)
; Changes:      a
; Returns:      a - file descriptor
; Flags:        Carry clear if ok, set otherwise
; ****************************************************************************
KFILEOPEN:
    jsr kWaitForFileCmd

    sta FILEIO::MODE

    stx FNAMEIN_VECTOR
    sty FNAMEIN_VECTOR+1

    ldy #$00
@nextNameChar:
    lda (FNAMEIN_VECTOR),y
    sta FILEIO::FILENAME,y
    beq @gotFileName

    iny
    bra @nextNameChar

@gotFileName:
    lda #FILEIO_OPEN
    sta FILEIO::CMD

    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    lda FILEIO::FD

    rts


; ****************************************************************************
; Function:     KFILECLOSE
; Description:  Close a file
; Input:        a - file descriptor
; Changes:      a
; Returns:      nothing
; Flags:        Carry clear if ok, set otherwise
; ****************************************************************************
KFILECLOSE:
    jsr kWaitForFileCmd

    sta FILEIO::FD
    lda #FILEIO_CLOSE
    sta FILEIO::CMD

    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    rts


; ****************************************************************************
; Function:     KFILEREAD
; Description:  Read a byte from a file
; Input:        a - file descriptor
; Changes:      a
; Returns:      a - byte read
; Flags:        Carry clear if ok, set othwerwise
; ****************************************************************************
KFILEREAD:
    sta FILEIO::FD
    lda #FILEIO_READ
    sta FILEIO::CMD
    
    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    lda FILEIO::DATA_LO

    rts


; ****************************************************************************
; Function:     KFILEWRITE
; Description:  Write a byte to a file
; Input:        a - byte to write
;               x - file descriptor
; Changes:      a
; Returns:      nothing
; Flags:        Carry clear if ok, set othwerwise
; ****************************************************************************
KFILEWRITE:
    stx FILEIO::FD
    sta FILEIO::DATA_LO
    lda #FILEIO_WRITE
    sta FILEIO::CMD

    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    rts


; ****************************************************************************
; Function:     KFILEFLUSH
; Description:  Flush file output
; Input:        a - file descriptor
; Changes:      a
; Returns:      nothing
; Flags:        Carry clear if ok, set othwerwise
; ****************************************************************************
KFILEFLUSH:
    sta FILEIO::FD
    lda #FILEIO_FLUSH
    sta FILEIO::CMD

    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    rts


; ****************************************************************************
; Function:     KFILEDELETE
; Description:  Delete a file
; Input:        x - filename (lo)
;               y - filename (hi)
; Changes:      a,y
; Returns:      nothing
; Flags:        Carry clear if ok, set otherwise
; ****************************************************************************
KFILEDELETE:
    jsr kWaitForFileCmd

    stx FNAMEIN_VECTOR
    sty FNAMEIN_VECTOR+1

    ldy #$00
@nextNameChar:
    lda (FNAMEIN_VECTOR),y
    sta FILEIO::FILENAME,y
    beq @gotFileName

    iny
    bra @nextNameChar

@gotFileName:
    lda #FILEIO_DELETE
    sta FILEIO::CMD

    jsr kDoFileCommand

    lda FILEIO::STATUS
    cmp #$01

    rts


; ****************************************************************************
; Function:     KFILESTATUS
; Description:  Get status of last file operation
; Input:        nothing
; Changes:      a
; Returns:      a - file status
; Flags:        nothing
; ****************************************************************************
KFILESTATUS:
    lda FILEIO::STATUS

    rts
