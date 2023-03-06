\ *************************************************************************** /
\ CORE Extension Words                                                        /
\ *************************************************************************** /

CR .( Loading CORE Extensions... )

: PARSE\"           ( -- addr len )
    0 TEMP$ C!
    SOURCE >IN @ /STRING 2DUP
    BEGIN   DUP
    WHILE   OVER C@ DUP
            [CHAR] \ =
            IF      OVER 1 =
                    ABORT" trailing '\' in string"
                    DROP >R 1+ R> 1- OVER C@
                    CASE
                        [CHAR] a OF   7 TEMP$ C+PLACE ENDOF
                        [CHAR] b OF   8 TEMP$ C+PLACE ENDOF
                        [CHAR] e OF  27 TEMP$ C+PLACE ENDOF
                        [CHAR] f OF  12 TEMP$ C+PLACE ENDOF
                        [CHAR] l OF  10 TEMP$ C+PLACE ENDOF
                        [CHAR] m OF  13 TEMP$ C+PLACE
                                     10 TEMP$ C+PLACE ENDOF
                        [CHAR] n OF  10 TEMP$ C+PLACE ENDOF
                        [CHAR] q OF  34 TEMP$ C+PLACE ENDOF
                        [CHAR] r OF  13 TEMP$ C+PLACE ENDOF
                        [CHAR] t OF   9 TEMP$ C+PLACE ENDOF
                        [CHAR] v OF  11 TEMP$ C+PLACE ENDOF
                        [CHAR] " of  34 TEMP$ C+PLACE ENDOF
                        [CHAR] \ of  92 TEMP$ C+PLACE ENDOF
                               DUP TEMP$ C+PLACE
                    ENDCASE
            ELSE    DUP [CHAR] " =
                    IF      DROP >R 1+ R> 1-
                            NIP - 1+ >IN +! DROP
                            TEMP$ COUNT EXIT
                    ELSE    TEMP$ C+PLACE
                    THEN
            THEN    >R 1+ R> 1-
    REPEAT  NIP - 1+ >IN +! DROP TEMP$ COUNT ;

: ,\"               ( -<cstring">- )
    PARSE\" DUP C, HERE >R DUP ALLOT R> SWAP MOVE ALIGN ;

: S\"               ( -<cstring">- )
    STATE @
    IF      POSTPONE (S") ,\"
    ELSE    PARSE\"
    THEN    ; IMMEDIATE
