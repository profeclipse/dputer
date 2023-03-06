\ ========================================================================== /
\ Find source file for word                                                  /
\ ========================================================================== /

: GET-VIEWFILE      ( cfa -- cfa loadfile true | false )
    DUP >NAME >R
    LOADFILE
    BEGIN   @ ?DUP
    WHILE   DUP R@ U<
            IF      CELL+ TRUE R>DROP EXIT
            THEN
    REPEAT  DROP R>DROP FALSE ;

\ ========================================================================== /
\ execution chain words                                                      /
\ ========================================================================== /

VARIABLE CHAIN-LINK
CHAIN-LINK OFF

: NEW-CHAIN         ( -- )
    CREATE 0 , ['] NOOP , CHAIN-LINK LINK, ;

: .CHAIN            ( chain -- )
    DUP @ 0=
    IF      DROP ." Empty"
    ELSE    BEGIN   @ ?DUP
            WHILE   DUP CELL+ @
                    >NAME DUP NFA-COUNT NIP 3 + ?CR
                    GETXY DROP 24 < COLS 48 > AND
                    IF      24 COL
                    THEN    .ID
                    START/STOP
            REPEAT
    THEN    ;

: .CHAINS           ( -- )
    CHAIN-LINK
    BEGIN   @ ?DUP
    WHILE   DUP 2 CELLS -
            CR DUP BODY> .NAME 23 COL SPACE .CHAIN
    REPEAT  CR ;

: DO-CHAIN          ( chain -- )
    BEGIN   @ ?DUP
    WHILE   DUP>R CELL+ @ EXECUTE R>
    REPEAT  ;

: NOOP-CHAIN-ADD    ( chain -- addr )
    BEGIN   DUP @
    WHILE   @
    REPEAT  HERE SWAP ! 0 , HERE ['] NOOP , ;

: CHAIN-ADD         ( chain -<word>- )
    ' >R
    BEGIN   DUP @
    WHILE   @
    REPEAT  HERE SWAP ! 0 , R> , ;

: CHAIN-ADD-BEFORE  ( chain -<word>- )
    ' >R HERE OVER @ , R> , SWAP ! ;

\ ========================================================================== /
\ some chains we will need                                                   /
\ ========================================================================== /

NEW-CHAIN INITIALIZATION-CHAIN  \ chain of things to initialize
NEW-CHAIN        NUMBER?-CHAIN  \ chain of number conversion options
NEW-CHAIN         FORGET-CHAIN  \ chain of types of things to forget
NEW-CHAIN      SEMICOLON-CHAIN  \ chain of things to do at end of definition
NEW-CHAIN          LEDIT-CHAIN  \ chain of line-edit functions
NEW-CHAIN    RESET-STACK-CHAIN  \ chain of stacks to reset

: _DO-;CHAIN        ( -- )
    SEMICOLON-CHAIN DO-CHAIN ;

' _DO-;CHAIN IS DO-;CHAIN

RESET-STACK-CHAIN CHAIN-ADD (RESET-STACKS)

: _DO-RESET-STACK-CHAIN ( -- )
    RESET-STACK-CHAIN DO-CHAIN ;

' _DO-RESET-STACK-CHAIN IS RESET-STACKS

\ ========================================================================== /
\ double-number plus-store                                                   /
\ ========================================================================== /

: 2+!               ( d1 a1 -- )
    DUP>R 2@ D+ R> 2! ;

\ ========================================================================== /
\ miscellaneous stuff                                                        /
\ ========================================================================== /

: EXEC:             ( n1 -- )
    CELLS R> + @ EXECUTE ;

0 VALUE OLDDEPTH

: NOSTACK1          ( -- )
    DEPTH TO OLDDEPTH ;

SYNONYM CHECKSTACK NOSTACK1

: NOSTACK           ( -- )
    -1 TO OLDDEPTH ;

: STACK-EMPTY?      ( -- )
    DEPTH ABORT" the stack should have been empty here" ;

: _STACK-CHECK      ( -- )
    ?LOADING @ 0= STATE @ OR OLDDEPTH 0< OR ?EXIT
    DEPTH OLDDEPTH >
    IF      CR ." stack depth increased in file: "
            LOADFILE @ CELL+ COUNT TYPE
            ."  at line: " BASE @ DECIMAL LOADLINE @ . BASE !
            ." stack: " .S CR
    THEN    DEPTH TO OLDDEPTH ;

NOSTACK
' _STACK-CHECK IS STACK-CHECK

DEFER SAVE-SOURCE ' NOOP IS SAVE-SOURCE

: ALLOT-TO          ( n -- )
    LAST @ NAME> >BODY HERE SWAP - - DUP 0<
    ABORT" buffer is already too long!"
    ALLOT ;

SYNONYM APP-ALLOT ALLOT

: APP-RESERVE       ( n1 -- )
    0MAX APP-HERE OVER ERASE APP-ALLOT ;

DEFER RESERVE ' APP-RESERVE IS RESERVE

: SECONDS           ( n -- )
    0MAX 0
    ?DO     10 0
            DO      100 MS
                    KEY?
                    IF      KEY DROP UNLOOP UNLOOP EXIT
                    THEN
            LOOP
    LOOP    ;

: WORD-SPLIT        ( n -- n'lo n'hi )
    DUP 255 AND SWAP 8 RSHIFT ;

: HIWORD            ( n -- n'hi )
    WORD-SPLIT NIP ;

: LOWORD            ( n -- n'lo )
    WORD-SPLIT DROP ;

: "HOLD             ( addr len -- )
    DUP NEGATE HLD +! HLD @ SWAP MOVE ;

