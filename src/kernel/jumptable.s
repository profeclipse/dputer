    .setcpu "65C02"

    .segment "JUMPTABLE"

MONITOR:        jmp KMONITOR
CHROUT:         jmp KWRITETERM
CHRIN:          jmp KGETCHAR
CHRUNGET:       jmp KUNREADTERM
CHRCHECK:       jmp KCHECKTERM
WRITESTR:       jmp KWRITESTR
GETSTR:         jmp KGETSTR
CLRSCR:         jmp KCLS
CURHOME:        jmp KHOME
SETCURX:        jmp KSETCURX
SETCURY:        jmp KSETCURY
SETCURXY:       jmp KSETCURXY
GETCURX:        jmp KGETCURX
GETCURY:        jmp KGETCURY
GETSCRW:        jmp KGETSCRW
GETSCRH:        jmp KGETSCRH
FILEOPEN:       jmp KFILEOPEN
FILECLOSE:      jmp KFILECLOSE
FILEREAD:       jmp KFILEREAD
FILEWRITE:      jmp KFILEWRITE
FILEFLUSH:      jmp KFILEFLUSH
FILEDELETE:     jmp KFILEDELETE
FILESTATUS:     jmp KFILESTATUS
FILEEXECIO:     jmp kDoFileCommand
FILEWAITIO:     jmp kWaitForFileCmd
