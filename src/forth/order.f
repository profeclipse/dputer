\ ************************************************************************** /
\ Search Order Words                                                         /
\ ************************************************************************** /

CR .( Loading Search Order Words... )

VARIABLE VOC-LINK
' FORTH >BODY VOC-LINK !

: WORDLIST
    VOC-LINK LINK, HERE 0 , 0 , ;

: VOCABULARY        ( -<vocname>- )
    CREATE WORDLIST DROP DOES> BODY> VCFA>VOC CONTEXT ! ;

VOCABULARY ROOT
\ ' ROOT @ ' FORTH !

: FORTH-WORDLIST    ( -- wid )
    ['] FORTH VCFA>VOC ;

: GET-CURRENT       ( -- wid )
    CURRENT @ ;

: SET-CURRENT       ( wid -- )
    CURRENT ! ;

: ALSO              ( -- )
    CONTEXT DUP CELL+ #VOCS 1- CELLS MOVE ;

: ONLY              ( -- )
    CONTEXT #VOCS CELLS ERASE ROOT ALSO ;

: DEFINITIONS       ( -- )
    CONTEXT @ CURRENT ! ;

: PREVIOUS          ( -- )
    CONTEXT DUP CELL+ SWAP #VOCS CELLS MOVE
    CONTEXT @ 0=
    IF      ROOT
    THEN    ;

: GET-ORDER         ( -- widn ... wid1 n )
    DEPTH >R
    0 #VOCS
    DO      CONTEXT I 1- CELLS+ @
            ?DUP DROP
    -1 +LOOP
    DEPTH R> - ;

: SET-ORDER         ( widn ... wid1 n -- )
    DUP 0<
    IF      DROP ONLY
    ELSE    CONTEXT #VOCS CELLS ERASE
            0 ?DO   CONTEXT I CELLS+ !
            LOOP
    THEN    ;

: .ORDER            ( -- )
    CR ." Context: "
    CONTEXT #VOCS 0
    DO      DUP @ ?DUP
            IF      VOC>VCFA .NAME 14 ?CR
            THEN    CELL+
    LOOP    DROP
    CR ." Current: " CURRENT @ VOC>VCFA .NAME ;

0 VALUE WORDS-CNT

: COUNT-VOC         ( voc -- )
    BEGIN   @ ?DUP
    WHILE   1 +TO WORDS-CNT N>LINK
    REPEAT  ;

: COUNT-WORDS       ( -- n )
    0 TO WORDS-CNT VOC-LINK
    BEGIN   @ ?DUP
    WHILE   DUP VLINK>VOC COUNT-VOC
    REPEAT  WORDS-CNT ;

: .VOCS             ( -- )
    CR ." Vocabularies             #Words"
    CR VOC-LINK @
    BEGIN   DUP VLINK>VOC
            DUP VOC>VCFA .NAME 23 #TAB
            0 TO WORDS-CNT COUNT-VOC WORDS-CNT 8 .R
            CR @ DUP 0=
    UNTIL   DROP
       ." -------------------------------"
    CR ." Total system words: " 22 #tab count-words 9 .R
    CR ;

FORTH ALSO ROOT DEFINITIONS

: FORTH             FORTH ;
: FORTH-WORDLIST    FORTH-WORDLIST ;
: SET-ORDER         SET-ORDER ;

ONLY FORTH ALSO DEFINITIONS

VOCABULARY HIDDEN
VOCABULARY EDITOR
VOCABULARY ASSEMBLER

