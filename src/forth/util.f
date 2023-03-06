\ ************************************************************************** /
\ Utility Words                                                              /
\ ************************************************************************** /

CR .( Loading Utility Words... )

: UMIN          ( u1 u2 -- f )      2DUP U> IF SWAP THEN DROP ;
: UMAX          ( u1 u2 -- f )      2DUP U< IF SWAP THEN DROP ;

: <=            ( n1 n2 -- f )      > 0= ;

: D<            ( d1 d2 -- f )      D- NIP 0< ;
: D>            ( d1 d2 -- f )      2SWAP D< ;
: DMIN          ( d1 d2 -- d )      2OVER 2OVER D> IF 2SWAP THEN 2DROP ;
: DMAX          ( d1 d2 -- d )      2OVER 2OVER D< IF 2SWAP THEN 2DROP ;
: D0=           ( d -- f )          OR 0= ;
: D=            ( d1 d2 -- f)       D- D0= ;

: 2CONSTANT  HEADER DO2CON , , , ;

: (COL)         ( n -- )            COLS 1- MIN GETXY DROP - SPACES ;

DEFER COL ' (COL) IS COL

: #TAB              ( n -- )
                    GETXY DROP OVER / 1+ * COL ;

: (?CR)             ( n -- )
                    GETXY DROP + COLS < 0=
                    IF      CR
                    THEN    ;

DEFER ?CR ' (?CR) IS ?CR

: .ID               ( nfa -- )  NFA-COUNT TYPE SPACE ;

: IN-DICT?          ( addr -- f )
    FORTH-KERNEL-START HERE WITHIN ;

: .NAME             ( xt -- )
    DUP IN-DICT?
    IF      DUP 2 CELLS - IN-DICT?
            IF      DUP >NAME IN-DICT?
                    IF      DUP DUP >NAME DUP>R NAME> DUP IN-DICT?
                            IF      =
                                    IF      R> .ID DROP EXIT
                                    ELSE    R>DROP
                                    THEN
                            ELSE    2DROP
                            THEN
                    THEN
            THEN
    THEN    [CHAR] " EMIT ." 0x" 1 H.R [CHAR] " EMIT SPACE ;

: WAIT          ( -- )
                KEY 27 = THROW_ABORT ?THROW ;

16 VALUE SCREENDELAY

CREATE DELAYS     0 ,   4 ,  10 ,  16 ,  20 ,
                 40 ,  60 ,  90 , 120 , 200 ,

: (START/STOP)  ( -- )
                KEY?
                IF      KEY 10 DIGIT
                        IF      CELLS DELAYS + @ TO SCREENDELAY
                        ELSE    27 = THROW_ABORT ?THROW WAIT
                        THEN
                THEN    ;

DEFER START/STOP    ' (START/STOP) IS START/STOP

: NUF?          ( -- f )
                ['] START/STOP CATCH ;

: EMIT.         ( n -- )
                DUP BL 127 BETWEEN 0=
                IF      DROP [CHAR] .
                THEN    EMIT ;

: +NO-WRAP      ( addr len -- addr' )
                0 TUCK D+ -1 0 DMIN DROP ;

: SEARCH-WORDLIST   ( addr len voc -- 0 | cfa flag )
                >R FIND-BUFFER PLACE FIND-BUFFER ?UPPERCASE COUNT
                R> @ SEARCH-ONE-WORDLIST ;

DEFER SAVE-SOURCE ' NOOP IS SAVE-SOURCE

: ASCII     CHAR        STATE @ IF POSTPONE LITERAL THEN ; IMMEDIATE
: CTRL      CHAR 31 AND STATE @ IF POSTPONE LITERAL THEN ; IMMEDIATE

: SYNONYM           ( -<newname>- -<oldname>- )
    HEADER DODEFER , COMPILE NOOP COMPILE NOOP
    DEFINED DUP 0= ?MISSING
    1 =
    IF      IMMEDIATE
    THEN    LAST @ NAME> >BODY ! ;

\ ========================================================================== /
\ Comments                                                                   /
\ ========================================================================== /

: _COMMENT      ( char -- )
                ?LOADING @
                IF      BEGIN   SOURCE >IN @ /STRING
                                2 PICK SCAN NIP 0=
                        WHILE   REFILL 0= ABORT" EOF encountered in comment"
                        REPEAT
                THEN    PARSE 2DROP ;

: COMMENT       ( -<chars>- )
                CHAR _COMMENT ; IMMEDIATE

TRUE VALUE MULTI-LINE?

: ML(               ( -- )
    MULTI-LINE?
    IF      [CHAR] ) _COMMENT
    ELSE    [CHAR] ) PARSE 2DROP
    THEN    ; IMMEDIATE

' ML( IS (

: ((                ( -- )
    BEGIN   BL WORD DUP COUNT S" ))" COMPARE
    WHILE   C@ 0=
            IF      REFILL 0= ABORT" missing ))"
            THEN
    REPEAT  DROP ; IMMEDIATE

: \S                ( -- )
    FALSE TO INCLUDING? [COMPILE] \ ; IMMEDIATE

: //                ( -- )
    SOURCE >IN ! DROP ; IMMEDIATE

: COMMENT:          ( -<comment;>- )
    BEGIN   BL WORD ?UPPERCASE
            DUP COUNT S" COMMENT;" COMPARE
    WHILE   C@ 0=
            IF      REFILL 0= ABORT" missing COMMENT;"
            THEN
    REPEAT  DROP ; IMMEDIATE

: \-                ( -<word>- )        \ load line if word is not defined
    defined nip
    if      postpone \
    then    ; immediate

: \+                ( -<word>- )        \ load line if word is defined
    defined nip 0=
    if      postpone \
    then    ; immediate

\ ========================================================================== /
\ Screen handling                                                            /
\ ========================================================================== /

 4 VALUE TAB-SIZE
 4 VALUE LEFT-MARGIN
 4 VALUE RIGHT-MARGIN
 0 VALUE TAB-MARGIN
10 VALUE TABS-MAX
 0 VALUE TABBING?
 0 VALUE FIRST-LINE?
-4 VALUE INDENT

: WRAP?             ( n -- f )
    GETCOLROW DROP RIGHT-MARGIN - > ;

: TAB-WRAP?         ( n -- f )
    DUP TABS-MAX TAB-SIZE * >
    SWAP WRAP? OR ;

: TAB               ( -- )
    GETXY DROP TAB-SIZE / 1+ TAB-SIZE * COL ;

: 0TAB              ( -- )
    0 TO TAB-MARGIN ;

: +TAB              ( -- )
    TAB-SIZE +TO TAB-MARGIN
    TAB-MARGIN TAB-WRAP? IF 0TAB THEN ;

: -TAB              ( -- )
    TAB-MARGIN TAB-SIZE - 0 MAX DUP TO TAB-MARGIN
    TAB-SIZE <
    IF      TABS-MAX TAB-SIZE * TO TAB-MARGIN
    THEN    ;

: FIRST-LINE        ( -- )
    TRUE TO FIRST-LINE? 0TAB ;

: TABBING-ON        ( -- )
    TRUE TO TABBING? ;

: TABBING-OFF       ( -- )
    FALSE TO TABBING? ;

: CRTAB             ( -- )
    (CR)
    TABBING? 0= ?EXIT
    FIRST-LINE?
    IF      LEFT-MARGIN INDENT + SPACES
            FALSE TO FIRST-LINE?
    ELSE    LEFT-MARGIN SPACES TAB-MARGIN SPACES
    THEN    ;

: ?LINE             ( n -- )
    0 MAX GETXY DROP + WRAP?
    IF CR THEN ;

' CRTAB IS CR

: HORIZONTAL-LINE   ( -- )
    GETCOLROW DROP GETXY DROP - 8 - 0MAX 8 /MOD 0
    ?DO     ." --------"
    LOOP    S" --------" DROP SWAP TYPE
    CR ;

\ ========================================================================== /
\ number output stuff
\ ========================================================================== /

: (.)               ( n1 -- a1 n1 )
    S>D (D.) ;

: U.R               ( u w -- )
    0 SWAP D.R ;

: (XUD,.)           ( ud n -- a1 n1 )
    >R
    <#
        R@ 0 DO # 2DUP D0= ?LEAVE LOOP
        BEGIN   2DUP D0= 0=
        WHILE   [CHAR] , HOLD
                R@ 0 DO # 2DUP D0= ?LEAVE LOOP
        REPEAT
    #>
    R> DROP ;

: (UD,.)            ( ud -- a1 n1 )
    BASE @
    DUP 10 = SWAP 8 = OR
    4 + (XUD,.) ;

: UD,.R             ( ud n -- )
    >R (UD,.) R> OVER - SPACES TYPE ;

: U,.R              ( n1 n2 -- )
    0 SWAP UD,.R ;

: U,.               ( n1 -- )
    0 U,.R ;

: UD.               ( ud -- )
    0 UD,.R SPACE ;

: UD.R              ( ud n -- )
    >R 16 (XUD,.) R> OVER - SPACES TYPE ;
 
: (D.#)             ( d1 n1 -- a1 n1 )
    >R <# R> ?DUP
    IF      0 MAX 0 ?DO # LOOP [CHAR] . HOLD
    THEN    #S #> ;

: D.R.#             ( d1 n1 n2 -- )
    SWAP >R (D.#) R> OVER - SPACES TYPE ;

: .R.1              ( n1 n2 -- )
    0 SWAP 1 D.R.# ;

: D.#               ( d1 n1 -- )
    1 SWAP D.R.# ;

: .#                ( n1 n2 -- )
    0 SWAP D.# ;

: B. BASE @ >R BINARY 0 <# 8 0 DO # LOOP BL HOLD 8 0 DO # LOOP #> TYPE R> BASE ! ;
: DB. B. SPACE B. ;


: (VERSION)
    VERSION# 0 <# # # [CHAR] . HOLD # # [CHAR] . HOLD #S #> ;

: .VERSION
    BASE @ DECIMAL ." Version " (VERSION) TYPE BASE ! ;

\ ========================================================================== /
\ Deferred word utilities                                                    /
\ ========================================================================== /

\ Restore a deferred word to its default function
: (RESTORE-DEFAULT) ( -- )
    @(IP) >BODY DUP 2 CELLS+ @ SWAP ! ;

: RESTORE-DEFAULT   ( -<name>- )
    STATE @
    IF      POSTPONE (RESTORE-DEFAULT)
    ELSE    ' >BODY DUP 2 CELLS+ @ SWAP !
    THEN    ; IMMEDIATE

\ Display deferred words and their current and default actions
: .DEFERRED         ( -- )
    DEFER-LIST @
    BEGIN   ?DUP
    WHILE   CR \ ." Deferred: "
            DUP CELL - DUP BODY> .NAME
            20 COL ."  does: " @ .NAME
            44 COL ."  default: " DUP CELL+ @ .NAME
            @
            START/STOP SCREENDELAY MS
    REPEAT  ;

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
\ File Loading Utilities                                                     /
\ ========================================================================== /

\ list loaded files
: .LOADED ( -- )
    LOADFILE @
    BEGIN   ?DUP
    WHILE   DUP CELL+ COUNT CR TYPE @
    REPEAT  ;

\ true if specified file is loaded
: ?LOADED ( addr len -- f )
    ( "to-pathend" ) 2>R LOADFILE @
    BEGIN   ?DUP
    WHILE   DUP CELL+ COUNT ( "to-pathend" ) 2r@ COMPARE 0=
            IF  2R> 2DROP DROP TRUE EXIT
            THEN @
    REPEAT  2r> 2DROP FALSE ;

\ load file if it's not already loaded
: REQUIRED ( addr len -- )
    2DUP ?LOADED 0=
    IF      INCLUDED
    ELSE    2DROP
    THEN    ;

\ load file if it's not already loaded
: REQUIRE ( -<name>- -- )
    BL WORD COUNT REQUIRED ;

\ ========================================================================== /
\ double-number plus-store                                                   /
\ ========================================================================== /

: 2+!               ( d1 a1 -- )
    DUP>R 2@ D+ R> 2! ;

\ ========================================================================== /
\ Create a callable cfa
\ ========================================================================== /

: CFA-FUNC          ( -<name>- )
    CREATE HIDE !CSP DODOES CALL, ] ;

\ ========================================================================== /
\ 2VALUE words                                                               /
\ ========================================================================== /

CFA-FUNC DO2VALUE!  2 CELLS - @ 2!  ;
CFA-FUNC DO2VALUE+! 3 CELLS - @ 2+! ;

: 2VALUE            ( d1 -<valuename>- )
    HEADER DO2VALUE , HERE 3 CELLS+ , DO2VALUE! , DO2VALUE+! , , , ;

: 2TO               ( d1 -<valuename>- )
    POSTPONE TO ; IMMEDIATE

: 2+TO              ( d1 -<valuename>- )
    POSTPONE +TO ; IMMEDIATE

\ ========================================================================== /
\ Welcome                                                                    /
\ ========================================================================== /

: (WELCOME)
    CLS
    ." *** DForth 65c02 " .VERSION
    CR ." *** " UNUSED 0 UD. ." bytes free" CR ;

' (WELCOME) IS WELCOME
