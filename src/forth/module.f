\ ************************************************************************** /
\ Module Words                                                               /
\ ************************************************************************** /

CR .( Loading Module Wordset...)

ONLY FORTH ALSO HIDDEN ALSO DEFINITIONS

0 VALUE PRE-VOC

FORTH DEFINITIONS

: INTERNAL          ( -- )
    CONTEXT @ ['] HIDDEN VCFA>VOC =
    IF      HIDDEN DEFINITIONS
    ELSE    CURRENT @ TO PRE-VOC
            ALSO HIDDEN DEFINITIONS
    THEN    ;

: EXTERNAL          ( -- )
    PRE-VOC 0= ABORT" use only while building a module"
    PRE-VOC CURRENT ! ;

: MODULE            ( -- )
    EXTERNAL
    CONTEXT @ ['] HIDDEN VCFA>VOC =
    IF PREVIOUS THEN 0 TO PRE-VOC ;

ONLY FORTH ALSO DEFINITIONS

((

INTERNAL

.. internal definitions ..

EXTERNAL

.. externally available definitions ..

MODULE

.. back to whatever vocabulary we started in ..

))

