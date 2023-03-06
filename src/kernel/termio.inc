;*****************************************************************************
; termio.inc
;*****************************************************************************

; terminal data locations
TERMIO_BASE     	= $DC00
TERMIO_IREADY   	= TERMIO_BASE+0
TERMIO_IDATA    	= TERMIO_BASE+1
TERMIO_OREADY   	= TERMIO_BASE+2
TERMIO_ODATA    	= TERMIO_BASE+3
TERMIO_CREADY		= TERMIO_BASE+4
TERMIO_CCMD			= TERMIO_BASE+5
TERMIO_CDATA		= TERMIO_BASE+6

TERMIO_CMD_CLS 		= 1
TERMIO_CMD_HOME 	= 2
TERMIO_CMD_CURSOR_X	= 3
TERMIO_CMD_CURSOR_Y	= 4
TERMIO_CMD_SCREEN_W	= 5
TERMIO_CMD_SCREEN_H	= 6
TERMIO_CMD_GETCUR_X	= 7
TERMIO_CMD_GETCUR_Y	= 8
