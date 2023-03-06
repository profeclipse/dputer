\ ************************************************************************** /
\ Forget                                                                     /
\ ************************************************************************** /

cr .( Loading Forget Wordset...)

VARIABLE FENCE

: _TRIM?
    <= ;

DEFER TRIM? ' _TRIM? IS TRIM?

: FULL-TRIM         ( nfa link -- )
    BEGIN   DUP @
    WHILE   2DUP @ TRIM?
            IF      DUP @ @ OVER !
            ELSE    @
            THEN
    REPEAT  2DROP ;

: TRIM-CHAINS       ( nfa -- nfa )
    CHAIN-LINK
    BEGIN   @ ?DUP
    WHILE   2DUP -2 CELLS+ FULL-TRIM
    REPEAT  DUP CHAIN-LINK FULL-TRIM ;

: TRIM-LOADFILE     ( nfa -- nfa )
    DUP LOADFILE FULL-TRIM ;

FORGET-CHAIN CHAIN-ADD TRIM-LOADFILE

: TRIM-DEFER        ( nfa -- nfa )
    DUP DEFER-LIST FULL-TRIM
    DEFER-LIST
    BEGIN   @ ?DUP
    WHILE   2DUP CELL- @ TRIM?
            IF      DUP CELL+ @ OVER CELL- !
            THEN
    REPEAT  ;

FORGET-CHAIN CHAIN-ADD TRIM-DEFER

: VTRIM             ( nfa voc -- )
    BEGIN   2DUP @ TRIM?
    WHILE   DUP DUP @ N>LINK @ SWAP !
    REPEAT  2DROP ;

: (FORGET)          ( cfa -- )
    >NAME DUP FENCE @ 1- TRIM? ABORT" in protected dictionary"
    VOC-LINK 2DUP FULL-TRIM
    BEGIN   @ ?DUP
    WHILE   2DUP VLINK>VOC VTRIM
    REPEAT  FORGET-CHAIN DO-CHAIN TRIM-CHAINS
    CONTEXT #VOCS CELLS+ CONTEXT
    DO      DUP I @ TRIM?
            IF      [ ' FORTH VCFA>VOC ] LITERAL I !
            THEN
    CELL +LOOP
    ( CELL- ) DP ! ;

: FORGET            ( -<name>- )
    BL WORD COUNT CURRENT @ SEARCH-WORDLIST 0= ?MISSING (FORGET) ;

: DO-MARK           ( -<name>- )
    DOES>
        MAXSTRING LOCALALLOC >r
        S" MARK " R@ PLACE
        BODY> DUP >NAME NFA-COUNT R@ +PLACE
        (FORGET) FORTH DEFINITIONS
        R> COUNT EVALUATE (LOCALFREE) ;

: MARK              ( -<name>- )
    CREATE SAVE-SOURCE DO-MARK ;

: DO-MARKER         ( -- )
    DOES>   DUP>R @ (FORGET)
            R> CELL+ DUP @ CURRENT !
            CELL+ CONTEXT #VOCS CELLS MOVE ;

: MARKER            ( -<name>- )
    CREATE SAVE-SOURCE
    HERE BODY> , CURRENT @ ,
    CONTEXT HERE #VOCS CELLS ALLOT #VOCS CELLS MOVE
    DO-MARKER ;

: ANEW              ( -<name>- )
    >IN @ DEFINED
    IF      EXECUTE
    ELSE    DROP
    THEN    >IN ! MARKER ;

