\ see.4th

cr .( Loading Decompiler...)

only forth also definitions
decimal

new-chain .execution-class-chain
new-chain .other-class-chain
new-chain .word-chain

0 value debugging?

defer (see)

INTERNAL

: .word             ( IP -- IP' )
    dup @ dup forth-kernel-start here within
    if      dup >r @
            case
                dovalue     of r@                     .name endof
                dovalue!    of r@ ." TO "   2 cells - .name endof
                dovalue+!   of r@ ." +TO "  3 cells - .name endof
                do2value!   of r@ ." 2TO "  2 cells - .name endof
                do2value+!  of r@ ." 2+TO " 3 cells - .name endof
\+ dolocal      dolocal     of r@                     .name endof
\+ dolocal!     dolocal!    of r@ ." TO "   2 cells - .name endof
\+ dolocal+!    dolocal+!   of r@ ." +TO  " 3 cells - .name endof
                            r@ false .word-chain do-chain 0=
                            if .name else drop then
            endcase
            r> drop
    else    ( ." 0x" ) 1 u.r space
    then    cell+ ;

: .branch           ( IP -- IP' )
    .word dup @ over - dup 0> if ." +" then h. cell+ ;

: .string           ( IP -- IP' )
    ascii " emit space cell+
    dup c@ ?line
    count 2dup type ascii " emit space + aligned ;

: .locals           ( IP -- IP' )
    ." INIT-LOCALS "
    cols ?line ." LOCALS|"
    dup 1+ c@ dup 0
    ?do     ."  LOCAL" i 2 pick c@ + 1 .r
    loop    drop dup c@ ?dup
    if      ."  \" dup 0
            ?do     ."  LOCAL" i 1 .r
            loop    drop
    then    ."  | " cols ?line cell+ ;

\ hex E8909090 decimal constant does-op
hex 20EA0269. decimal 2constant does-op

: does?             ( IP -- IP+ flag )
    dup 2 cells+ swap 2@ does-op d= ;

: .(;code)          ( IP -- flag )
    cell+ does?
    if      ." DOES> "
    else    ." ;CODE " drop false
    then    ;

: .(does>)          ( IP -- IP' )
    cell+ cell+ ." DOES> " ;

: d_cr              ( -- )
    debugging? 0= if crtab then ;

0 value hi-branch

: branch+           ( IP -- IP' )
    cell+ dup @ hi-branch umax to hi-branch ;

: .end              ( IP -- IP' | 0 )
    cell+ dup hi-branch u< 0=
    if      ." ; " drop 0
    else    ." EXIT"
    then    ;

: .execution-class  ( IP cfa -- IP' )
    case
        ['] lit         of cell+     ( ." lit " ) .word              endof
        ['] (is)        of cell+     ." (IS) "    .word              endof
        ['] (.")        of           ." ."        .string            endof
        ['] (s")        of           ." S"        .string            endof
        ['] (c")        of           ." C"        .string            endof
        ['] (abort")    of           ." ABORT"    .string            endof
        ['] ?branch     of      d_cr ." IF      " +tab branch+ cell+ endof
\+ -?branch        ['] -?branch    of      d_cr ." -IF     " +tab branch+ cell+ endof
        ['] branch      of -tab d_cr ." ELSE    " +tab branch+ cell+ endof
        ['] (do)        of      d_cr ." DO      " +tab branch+ cell+ endof
        ['] (?do)       of      d_cr ." ?DO     " +tab branch+ cell+ endof
        ['] (loop)      of -tab d_cr ." LOOP    "      cell+   cell+ endof
        ['] (+loop)     of -tab d_cr ." +LOOP   "      cell+   cell+ endof
        ['] (case)      of      d_cr ." CASE    " +tab cell+         endof
        ['] (of)        of      d_cr ." OF      " +tab branch+ cell+ endof
        ['] (endof)     of  tab      ." ENDOF   " -tab d_cr
                                                       branch+ cell+ endof
        ['] (endcase)   of -tab d_cr ." ENDCASE "              cell+ endof
        ['] (then)      of -tab d_cr ." THEN    "              cell+ endof
        ['] (begin)     of      d_cr ." BEGIN   " +tab cell+         endof
        ['] (while)     of -tab d_cr ." WHILE   " +tab branch+ cell+ endof
        ['] (until)     of -tab d_cr ." UNTIL   "      cell+   cell+ endof
        ['] (repeat)    of -tab d_cr ." REPEAT  "      cell+   cell+ endof
        ['] (again)     of -tab d_cr ." AGAIN   "      cell+   cell+ endof
        ['] compile     of           .word .word                     endof
        ['] unnest      of           .end space                      endof
\+ unnestp        ['] unnestp     of           .end space                      endof
        ['] (;code)     of -tab d_cr .(;code)      tab +tab          endof
        ['] create      of d_cr      ." CREATE"    tab +tab    cell+ endof
\+ init-locals        ['] init-locals of cell+     .locals                         endof
        false .execution-class-chain do-chain 0=
        if      swap .word swap
        then
    endcase ;

: .pfa              ( cfa -- )
    0 to hi-branch
    tabbing-on
    0tab +tab tab
    begin   14 ?line dup @ .execution-class
            dup 0= nuf? or
    until   drop tabbing-off ;

: .immediate        ( cfa -- )
    >name c@ IMMEDBIT and if ." IMMEDIATE " then ;

: .constant         ( cfa -- )
    dup >body ? ." CONSTANT " .name ;

\+ fconstant : .fconstant ( cfa )
\+ fconstant    dup >body f@ fe. ." FCONSTANT " .name ;

: .offset           ( cfa -- )
    dup >body ? ." OFFSET " .name ;

: .user             ( cfa -- )
    dup >body dup . @ . ." USER " dup .name
    ." Value = " execute ? ;

: .variable         ( cfa -- )
    dup >body . ." VARIABLE " dup .name
    ." Value = " >body ? ;

: .value            ( cfa -- )
    dup cell+ ? ." VALUE " .name ;

: .2value           ( cfa -- )
    dup cell+ @ 2@ (d.) type ." . 2VALUE " .name ;

\+ fvalue : .fvalue ( cfa -- )
\+ fvalue     dup cell+ @ f@ fe. ." FVALUE " .name ;

: .vocabulary       ( cfa -- )
    ." VOCABULARY " .name ;

: .:                ( cfa -- )
    ." : " dup .name 2 spaces >body .pfa ;

: .defer            ( cfa -- )
    ." DEFER " dup .name ." IS " >is @ (see) ;

defer discode ' drop is discode

: .does>            ( pfa -- )
    \ ." DOES> " @ 3 + @ cell- .pfa ;
    ." DOES> " .pfa ;

: .code             ( cfa -- )
    ." IS CODE " @ discode ;

: .;code            ( cfa -- )
    ." IS ;CODE " @ discode ;

: .other            ( cfa -- )
    dup .name
    .other-class-chain do-chain ?dup
    if      dup @ over cell+ = if .code exit then
            dup @ does? if .does> drop exit then
            drop .;code
    then    ;

: .definition-class ( cfa cfa -- )
    @
    case
             docolon   of .:          endof
             docon     of .constant   endof
\+ fconstant dofcon   of .fconstant  endof
             dovar     of .variable   endof
\+ douser    douser    of .user       endof
             dodefer   of .defer      endof
             dovalue   of .value      endof
             do2value  of .2value     endof
\+ dofvalue  dofvalue  of .fvalue     endof
             dovoc     of .vocabulary endof
             swap .other
    endcase ;

: ((see))           ( cfa -- )
    crtab dup dup .definition-class .immediate ;

' ((see)) is (see)

EXTERNAL

synonym 'see (see)

: see               ( -<name>- )
    ' (see) ;

MODULE
