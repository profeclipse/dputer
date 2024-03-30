\ ************************************************************************** /
\ Forth 65c02 Assembler                                                      /
\ ************************************************************************** /

cr .( Loading Assembler...)

ONLY FORTH DEFINITIONS ALSO ASSEMBLER DEFINITIONS
VOCABULARY ASM-HIDDEN ALSO ASM-HIDDEN DEFINITIONS ALSO ASSEMBLER

: IN-ASM ( all later words are defined in the assembler vocabulary )
        ONLY FORTH ALSO ASM-HIDDEN ALSO ASSEMBLER DEFINITIONS ;
: IN-HIDDEN ( all later words are defined in the hidden vocabulary )
        ONLY FORTH ALSO ASM-HIDDEN DEFINITIONS ALSO ASSEMBLER ;
: IN-FORTH ( all later words are defined in the forth vocabulary )
        ONLY FORTH DEFINITIONS ALSO ASM-HIDDEN ALSO ASSEMBLER ;

BASE @ HEX

IN-HIDDEN

CREATE INDEX
    0909 , 1505 , 0115 , 1211 , 8009 , 1D0D , 8019 , 8080 ,
    0080 , 1404 , 8014 , 8080 , 8080 , 1C0C , 3C1C , 2C80 ,
    6978 , 1404 , 8080 , 8080 , 8078 , 1C0C , 8080 , 8080 ,
    8080 , 1404 , 8080 , 8080 , 8080 , 3E3C , 8080 , 8080 ,

VARIABLE MODE 2 MODE !

IN-ASM

: ZP   2 MODE ! ;
: .A   0 MODE ! ;    : #     1 MODE ! ;     : MEM   ZP ;
: ,X   3 MODE ! ;    : ,Y    4 MODE ! ;     : X)    5 MODE  ! ;
: )Y   6 MODE ! ;    : )     F MODE ! ;

IN-HIDDEN
: ABYT CREATE C, DOES> C@ C, C, ;

IN-ASM
90 ABYT BCC, B0 ABYT BCS, F0 ABYT BEQ,  
30 ABYT BMI, D0 ABYT BNE, 10 ABYT BPL,  
50 ABYT BVC, 70 ABYT BVS, 80 ABYT BRA,

IN-HIDDEN

: ?WORD
    MODE @ 7 AND
    IF      OVER FF00 AND
    ELSE    0
    THEN ;
: B,
    DUP 0F AND
    IF      C, C,
    ELSE    C, DROP
    THEN ;
: UPMODE
    IF      MODE @ 8 OR MODE !
    THEN    1 MODE @ 0F AND ?DUP
    IF      0 DO DUP + LOOP
    THEN    OVER 2+ @ AND 0= ;

: S/C CREATE C, DOES> C@ C, MEM ;

IN-ASM

00 S/C BRK, 18 S/C CLC, D8 S/C CLD,
58 S/C CLI, B8 S/C CLV, CA S/C DEX,
88 S/C DEY, E8 S/C INX, C8 S/C INY,
EA S/C NOP, 48 S/C PHA, 08 S/C PHP,
68 S/C PLA, 28 S/C PLP, 40 S/C RTI,
60 S/C RTS, 38 S/C SEC, F8 S/C SED,
78 S/C SEI, AA S/C TAX, A8 S/C TAY,
BA S/C TSX, 8A S/C TXA, 9A S/C TXS,
98 S/C TYA,
5A S/C PHY, DA S/C PHX, CB S/C WAI,    
7A S/C PLY, FA S/C PLX, DB S/C STP,

IN-HIDDEN

: M/C
    CREATE , ,
    DOES>                 
        DUP 1+ C@ 10 * MODE +!
        ?WORD UPMODE UPMODE ABORT" invalid addressing mode"
        C@ MODE C@ INDEX + C@ + DUP 100 >
        IF      100 - DUP 5A =
                IF      DROP 1A
                THEN ( INC A CORR. ) 
        THEN    C, ( OPCODE )                  
        MODE C@ DUP 7 AND
        IF      ( OPERAND ) 0F AND 8 <
                IF      C,
                ELSE    ,
                THEN
        ELSE    DROP
        THEN    MEM ;

IN-ASM

0400 0114 M/C JSR, 0406 01E0 M/C CPX,
0406 01C0 M/C CPY, 040C 0180 M/C STY,
0414 0081 M/C STX, 0C0E 01A0 M/C LDY,
0D0D 0001 M/C ASL, 0D0D 0041 M/C LSR,
0D0D 0021 M/C ROL, 0D0D 0061 M/C ROR,
1416 01A2 M/C LDX, 1CEC 0080 M/C STA,
1CEE 0060 M/C ADC, 1CEE 0020 M/C AND,
1CEE 00C0 M/C CMP, 1CEE 0040 M/C EOR,
1CEE 00A0 M/C LDA, 1CEE 0000 M/C ORA,
1CEE 00E0 M/C SBC,
( CHANGES AGAINST 6502 )
A400 0140 M/C JMP, ( IND,X             )
0D0D 02C2 M/C DEC, ( .A                )
0D0D 02E2 M/C INC, ( .A                )
0C0E 0220 M/C BIT, ( IMM. ZP,X  ABS,X  )
0C0C 0360 M/C STZ, ( ZP ZP,X ABS ABS,X )
0404 00FF M/C TSB, ( TEST SET BIT:     )
                   ( ZP AB             )
0404 000F M/C TRB, ( TEST RESET BIT:   )
                   ( ZP AB             )

IN-HIDDEN

: BBIT
    CREATE C,
    DOES>
        C@ DUP 0F AND DUP
        IF      7 =
                IF      SWAP 10 * + C, C,
                ELSE    SWAP 10 * +
                THEN
        ELSE    SWAP
        THEN ;

IN-ASM

10 BBIT 0<   80 BBIT VS  ( BPL,BVC     )
90 BBIT CS   D0 BBIT 0=  ( BCC,BNE     )
90 BBIT >=               ( BCC ALIAS   )
30 BBIT 0>=  A0 BBIT VC  ( BMI,BVS     )
B0 BBIT CC   F0 BBIT 0<> ( BCS,BEQ     )
B0 BBIT <                ( BCS ALIAS   )
( 65C02 ROCKWELL CHANGES )
07 BBIT RMB, 87 BBIT SMB, ( RE-,SET MB )
0F BBIT BSET 8F BBIT BRESET ( BR ON "" )

: NOT DUP 0F AND IF 80 ELSE 20 THEN XOR ;
: ?RANGE   ( BRANCH -- BRANCH )
  DUP ABS 07F > ABORT" ? out of range " ;
 
IN-HIDDEN

VARIABLE DOFAR : FAR -1 DOFAR ! ;

TRUE VALUE IS65C02

IN-ASM

: CPU65C02 TRUE TO IS65C02 ;
: CPU6502 FALSE TO IS65C02 ;

: BEGIN,
    HERE 1 ; IMMEDIATE
: UNTIL,
    ?EXEC ROT 1 ?PAIRS HERE >R B,
    DUP HERE 1+ - DUP ABS 07F >
    IF    DROP >R C@ NOT R> C!  3 C, ( ABS.: BRANCH OVER JMP ) JMP,
    ELSE  ( REL. ) C, DROP R> DROP
    THEN  0 DOFAR ! ; IMMEDIATE
: AGAIN,
    1 ?PAIRS ( ABS. OR RELATIVE ) DUP HERE 2+ - DUP ABS 07F >
    IS65C02 [ FORTH ] 0= [ ASSEMBLER ] OR
    IF      DROP JMP, ELSE SWAP DROP BRA,
    THEN    0 DOFAR ! ; IMMEDIATE
: IF,
    DOFAR @
    IF      NOT B, 3 C, HERE 1+ 1 JMP,
    ELSE    B, HERE 0 C,
    THEN    0 DOFAR !  2 ; IMMEDIATE
: THEN,
    ?EXEC 2 ?PAIRS HERE OVER C@
    IF      SWAP !
    ELSE    OVER 1+ - ?RANGE SWAP C!
    THEN    0 DOFAR ! ;  IMMEDIATE
: ELSE,
    2 ?PAIRS HERE 1+ DOFAR @ IS65C02 [ FORTH ] 0= [ ASSEMBLER ] OR
    IF      1 JMP,
    ELSE    0 BRA,
    THEN    SWAP 2 [COMPILE] THEN, 2 ; IMMEDIATE
: WHILE,
    [COMPILE] IF, DROP 3 ; IMMEDIATE
: REPEAT,
    3 ?PAIRS >R [COMPILE] AGAIN, R> 2 [COMPILE] THEN, ; IMMEDIATE

\ : END-CCODE
\     CURRENT @ CONTEXT ! ?EXEC ?CSP ; IMMEDIATE

IN-HIDDEN

DEFER CODE-HEADER ' HEADER IS CODE-HEADER

VARIABLE SP-SAVE

: SAVE-DEPTH ( -- )
        SP@ SP-SAVE ! ;

: DEPTH-CHANGE ( report on a change of depth )
        SP@ SP-SAVE @ SWAP - 1 CELLS / ;

VARIABLE CURRENT-SV     ( needed for stashing the current vocabulary )
: SAVE-CURRENT          ( save the current vocabulary linkage )
        ( -- )
        CURRENT @ CURRENT-SV ! ;

: UNSAVE-CURRENT        ( reset current-sv )
        ( -- )
        0 CURRENT-SV ! ;

: RESTORE-CURRENT       ( restore current to its previously saved value )
        ( -- )
        CURRENT-SV @ ?DUP IF CURRENT ! UNSAVE-CURRENT THEN ;
 
: RESET-ASM SAVE-DEPTH ;

IN-ASM

: INIT-ASM
        ALSO ASSEMBLER RESET-ASM ;

: _CODE     ( start a native code definition )
        CODE-HEADER HERE CELL+ , HIDE MEM !CSP INIT-ASM ;

: _;CODE    ( create the [;code] part of a low-level defining word )
        ?CSP !CSP COMPILE (;CODE) POSTPONE [ INIT-ASM ;

IN-FORTH

DEFER CODE ' _CODE IS CODE
DEFER ;CODE ' _;CODE IS ;CODE IMMEDIATE

DEFER EXIT-ASSEMBLER ' NOOP IS EXIT-ASSEMBLER

: SUBR: ( create a subroutine in the assembler vocabulary )
        SAVE-CURRENT INIT-ASM DEFINITIONS !CSP CREATE HIDE HERE 0
        , ALIGN HERE SWAP ! DOES> @ ;

IN-ASM

: _LABEL
        ?EXEC CREATE MEM !CSP INIT-ASM ;

IN-FORTH

DEFER LABEL ' _LABEL IS LABEL IMMEDIATE

IN-ASM

: END-ASM PREVIOUS ;

IN-HIDDEN
: _END-CODE ( end a code definition )
        END-ASM ?CSP REVEAL RESTORE-CURRENT ALIGN
        EXIT-ASSEMBLER ;

IN-ASM
DEFER END-CODE  ' _END-CODE IS END-CODE
DEFER ;C        ' _END-CODE IS ;C

ONLY FORTH ALSO DEFINITIONS

BASE !

