\ ========================================================================== /
\ Programming Tools                                                          /
\ ========================================================================== /

CR .( Loading Programming Tools... )

DEFER DUMPC@ ' C@ IS DUMPC@

: DUMP          ( addr len -- )
                OVER +NO-WRAP DUP ROT
                ?DO     CR I H.4 SPACE SPACE
                        I 16 +NO-WRAP OVER UMIN I 2DUP
                        DO      I DUMPC@ H.2 SPACE I J 7 + = IF SPACE THEN
                        LOOP    2DUP - 16 OVER - 3 * SWAP 8 < - 1+ SPACES
                        DO      I DUMPC@ EMIT.
                        LOOP    NUF? ?LEAVE
                        SCREENDELAY MS 16
                +LOOP   DROP ;

: .S            ( -- )
                ?STACK
                DEPTH .SMAX @ MIN DUP
                IF      ." [" DEPTH 1- 1 .R ." ] "
                        BEGIN   DUP PICK 1 .R
                                BASE @ 16 =
                                IF      ." h"
                                THEN    SPACE
                                1- DUP 0=
                        UNTIL
                ELSE    ." Empty "
                THEN    DROP ;

