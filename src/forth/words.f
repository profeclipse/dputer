\ ************************************************************************** /
\ WORDS                                                                      /
\ ************************************************************************** /

CR .( Loading WORDS Extension...)

ONLY FORTH ALSO DEFINITIONS

INTERNAL

: (WORDS)           ( voc -- )
    CR DUP VOC>VCFA ." Vocabulary: " .NAME
    CR HORIZONTAL-LINE
    BEGIN   @ ?DUP
    WHILE   DUP NFA-COUNT DUP ?CR
            TYPE 22 #TAB
            N>LINK 1 +TO WORDS-CNT
            START/STOP SCREENDELAY MS
    REPEAT  ;

: DO-WORDS          ( voc -- )
    ['] (WORDS) CATCH
    IF      DROP CR ." Interrupted"
    THEN    ;

EXTERNAL

: .WORDS            ( -- )
    COUNT-WORDS
    CR 6 U,.R ."  words in the system." ;

: WORDS             ( -- )
    0 TO WORDS-CNT
    CONTEXT @ DO-WORDS
    BASE @ >R DECIMAL
    CR HORIZONTAL-LINE
    ." Displayed " WORDS-CNT . ." of the "
    COUNT-WORDS . ." words in the system."
    R> BASE ! ;

: ALLWORDS          ( -- )
    VOC-LINK @
    BEGIN   DUP VLINK>VOC 0 TO WORDS-CNT
            DUP DO-WORDS
            CR HORIZONTAL-LINE
            ." Displayed " WORDS-CNT . ." of the "
            0 TO WORDS-CNT COUNT-VOC WORDS-CNT . ." words in the vocabulary."
            @ DUP IF CR THEN DUP 0=
    UNTIL   DROP
    CR COUNT-WORDS . ." total words in the system." ;

MODULE

