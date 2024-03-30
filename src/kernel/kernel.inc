;*****************************************************************************
; kernel.inc
;*****************************************************************************

; zero page vectors
USER_ZP				= $00		; start of user zp space
RESERVED_ZP         = $D0       ; start of kernel reserved zp space

; general purpose data
KBD_BUFFER			= $0300		; 256 byte keyboard buffer
INPUT_BUFFER		= $0400		; 256 byte input buffer
INPUT_BUFFER_LEN	= $FF		; max length of input string

; BRK status bit mask
BRK_MASK            = %00010000


;*****************************************************************************
; TERMIO
;*****************************************************************************
    .struct TERMIO
    .org $0200
IREADY      .byte
IDATA       .byte
OREADY      .byte
ODATA       .byte
CREADY      .byte
CCMD        .byte
CDATA       .byte
    .endstruct

TERMIO_CLS          = 1
TERMIO_HOME         = 2
TERMIO_CURSOR_X     = 3
TERMIO_CURSOR_Y     = 4
TERMIO_SCREEN_W     = 5
TERMIO_SCREEN_H     = 6
TERMIO_GETCUR_X     = 7
TERMIO_GETCUR_Y     = 8


;*****************************************************************************
; FILEIO
;*****************************************************************************
    .struct FILEIO
    .org $0210
READY       .byte
CMD         .byte
FD          .byte
MODE        .byte
DATA_LO     .byte
DATA_HI     .byte
DATA_LO2    .byte
DATA_HI2    .byte
STATUS      .byte
DUMMY       .byte
FILENAME    .byte $E6
    .endstruct

FILEIO_OPEN         = 1
FILEIO_CLOSE        = 2
FILEIO_READ         = 3
FILEIO_WRITE        = 4
FILEIO_SEEK         = 5
FILEIO_DELETE       = 6
FILEIO_FILEPOS      = 7
FILEIO_FILESIZ      = 8
FILEIO_FLUSH        = 9
FILEIO_RESIZE       = 10

FILEIO_STATUS_OK    = 0
FILEIO_STATUS_EOF   = 1
FILEIO_STATUS_ERR   = 2

FILEIO_MODE_READ    = 1
FILEIO_MODE_WRITE   = 2
FILEIO_MODE_RW		= 3
FILEIO_MODE_BIN     = $80
