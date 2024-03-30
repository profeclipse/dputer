
; Enhanced BASIC $ver 2.22
; for DPuter

; zero page use ..

    .segment "ZEROPAGE"

    .org USER_ZP

LAB_WARM: 	.res 1		; BASIC warm start entry point
Wrmjpl: 		.res 1	; BASIC warm start vector jump low byte
Wrmjph: 		.res 1	; BASIC warm start vector jump high byte

Usrjmp:		.res 1		; USR function JMP address
Usrjpl:		.res 1		; USR function JMP vector low byte
Usrjph:		.res 1		; USR function JMP vector high byte
Nullct:		.res 1		; nulls output after each line
TPos:		.res 1		; BASIC terminal position byte
TWidth:		.res 1      ; BASIC terminal width byte
Iclim:		.res 1		; input column limit
Itempl:		.res 1	    ; temporary integer low byte
Itemph:		.res 1  	; temporary integer high byte

nums_1		= Itempl	; number to bin/hex string convert MSB
nums_2		= Itemph	; number to bin/hex string convert
nums_3:		.res 1	    ; number to bin/hex string convert LSB

Srchc:		.res 1	; search character
Temp3		= Srchc			; temp byte used in number routines
Scnquo:		.res 1	; scan-between-quotes flag
Asrch		= Scnquo		; alt search character

XOAw_l		= Srchc			; eXclusive OR, OR and AND word low byte
XOAw_h		= Scnquo		; eXclusive OR, OR and AND word high byte

Ibptr:		.res 1	; input buffer pointer
Dimcnt		= Ibptr			; # of dimensions
Tindx		= Ibptr			; token index

Defdim:		.res 1	; default DIM flag
Dtypef:		.res 1	; data type flag, $FF=string, $00=numeric
Oquote:		.res 1	; open quote flag (b7) (Flag: DATA scan; LIST quote; memory)
Gclctd:		.res 1	; garbage collected flag
Sufnxf:		.res 1	; subscript/FNX flag, 1xxx xxx = FN(0xxx xxx)
Imode:		.res 1	; input mode flag, $00=INPUT, $80=READ

Cflag:		.res 1	; comparison evaluation flag

TabSiz:		.res 1	; TAB step size (was input flag)

next_s:		.res 1	; next descriptor stack address

							; these two bytes form a word pointer to the item
							; currently on top of the descriptor stack
last_sl:		.res 1	; last descriptor stack address low byte
last_sh:		.res 1	; last descriptor stack address high byte (always $00)

des_sk:		.res 10	; descriptor stack start address (temp strings)

;			= USER_ZP+36	; End of descriptor stack

ut1_pl:		.res 1	; utility pointer 1 low byte
ut1_ph:		.res 1	; utility pointer 1 high byte
ut2_pl:		.res 1	; utility pointer 2 low byte
ut2_ph:		.res 1	; utility pointer 2 high byte

Temp_2		= ut1_pl		; temp byte for block move	

FACt_1:		.res 1	; FAC temp mantissa1
FACt_2:		.res 1	; FAC temp mantissa2
FACt_3:		.res 1	; FAC temp mantissa3

dims_l		= FACt_2		; array dimension size low byte
dims_h		= FACt_3		; array dimension size high byte

TempB:		.res 1	; temp page 0 byte

Smeml:		.res 1	; start of mem low byte		(Start-of-Basic)
Smemh:		.res 1	; start of mem high byte	(Start-of-Basic)
Svarl:		.res 1	; start of vars low byte	(Start-of-Variables)
Svarh:		.res 1	; start of vars high byte	(Start-of-Variables)
Sarryl:		.res 1	; var mem end low byte		(Start-of-Arrays)
Sarryh:		.res 1	; var mem end high byte		(Start-of-Arrays)
Earryl:		.res 1	; array mem end low byte	(End-of-Arrays)
Earryh:		.res 1	; array mem end high byte	(End-of-Arrays)
Sstorl:		.res 1	; string storage low byte	(String storage (moving down))
Sstorh:		.res 1	; string storage high byte	(String storage (moving down))
Sutill:		.res 1	; string utility ptr low byte
Sutilh:		.res 1	; string utility ptr high byte
Ememl:		.res 1	; end of mem low byte		(Limit-of-memory)
Ememh:		.res 1	; end of mem high byte		(Limit-of-memory)
Clinel:		.res 1	; current line low byte		(Basic line number)
Clineh:		.res 1	; current line high byte	(Basic line number)
Blinel:		.res 1	; break line low byte		(Previous Basic line number)
Blineh:		.res 1	; break line high byte		(Previous Basic line number)

Cpntrl:		.res 1	; continue pointer low byte
Cpntrh:		.res 1	; continue pointer high byte

Dlinel:		.res 1	; current DATA line low byte
Dlineh:		.res 1	; current DATA line high byte

Dptrl:		.res 1	; DATA pointer low byte
Dptrh:		.res 1	; DATA pointer high byte

Rdptrl:		.res 1	; read pointer low byte
Rdptrh:		.res 1	; read pointer high byte

Varnm1:		.res 1	; current var name 1st byte
Varnm2:		.res 1	; current var name 2nd byte

Cvaral:		.res 1	; current var address low byte
Cvarah:		.res 1	; current var address high byte

Frnxtl:		.res 1	; var pointer for FOR/NEXT low byte
Frnxth:		.res 1	; var pointer for FOR/NEXT high byte

Tidx1		= Frnxtl		; temp line index

Lvarpl		= Frnxtl		; let var pointer low byte
Lvarph		= Frnxth		; let var pointer high byte

prstk:		.res 2	; precedence stacked flag

comp_f:		.res 1	; compare function flag, bits 0,1 and 2 used
							; bit 2 set if >
							; bit 1 set if =
							; bit 0 set if <

func_l:		.res 1	; function pointer low byte
func_h:		.res 1	; function pointer high byte

garb_l		= func_l		; garbage collection working pointer low byte
garb_h		= func_h		; garbage collection working pointer high byte

des_2l:		.res 1	; string descriptor_2 pointer low byte
des_2h:		.res 1	; string descriptor_2 pointer high byte

g_step:		.res 1	; garbage collect step size

Fnxjmp:		.res 1	; jump vector for functions
Fnxjpl:		.res 1	; functions jump vector low byte
Fnxjph:		.res 1	; functions jump vector high byte

g_indx		= Fnxjpl		; garbage collect temp index

FAC2_r:		.res 1 ; FAC2 rounding byte

Adatal:		.res 1	; array data pointer low byte
Adatah:		.res 1	; array data pointer high  byte

Nbendl		= Adatal		; new block end pointer low byte
Nbendh		= Adatah		; new block end pointer high  byte

Obendl:		.res 1	; old block end pointer low byte
Obendh:		.res 1	; old block end pointer high  byte

;numexp:		.res 1	; string to float number exponent count
numexp      = Obendh	; string to float number exponent count
expcnt:		.res 1	; string to float exponent count

numbit		= numexp		; bit count for array element calculations

numdpf:		.res 1	; string to float decimal point flag
expneg:		.res 1	; string to float eval exponent -ve flag

Astrtl		= numdpf		; array start pointer low byte
Astrth		= expneg		; array start pointer high  byte

Histrl		= numdpf		; highest string low byte
Histrh		= expneg		; highest string high  byte

Baslnl		= numdpf		; BASIC search line pointer low byte
Baslnh		= expneg		; BASIC search line pointer high  byte

Fvar_l		= numdpf		; find/found variable pointer low byte
Fvar_h		= expneg		; find/found variable pointer high  byte

Ostrtl		= numdpf		; old block start pointer low byte
Ostrth		= expneg		; old block start pointer high  byte

Vrschl		= numdpf		; variable search pointer low byte
Vrschh		= expneg		; variable search pointer high  byte

FAC1_e:		.res 1	; FAC1 exponent
FAC1_1:		.res 1	; FAC1 mantissa1
FAC1_2:		.res 1	; FAC1 mantissa2
FAC1_3:		.res 1	; FAC1 mantissa3
FAC1_s:		.res 1	; FAC1 sign (b7)

str_ln		= FAC1_e		; string length
str_pl		= FAC1_1		; string pointer low byte
str_ph		= FAC1_2		; string pointer high byte

des_pl		= FAC1_2		; string descriptor pointer low byte
des_ph		= FAC1_3		; string descriptor pointer high byte

mids_l		= FAC1_3		; MID$ string temp length byte

negnum:		.res 1 	; string to float eval -ve flag
;numcon:		.res 1	; series evaluation constant count
numcon      = negnum	; series evaluation constant count

FAC1_o:		.res 1	; FAC1 overflow byte

FAC2_e:		.res 1	; FAC2 exponent
FAC2_1:		.res 1	; FAC2 mantissa1
FAC2_2:		.res 1	; FAC2 mantissa2
FAC2_3:		.res 1	; FAC2 mantissa3
FAC2_s:		.res 1	; FAC2 sign (b7)

FAC_sc:		.res 1	; FAC sign comparison, Acc#1 vs #2
FAC1_r:		.res 1	; FAC1 rounding byte

ssptr_l		= FAC_sc		; string start pointer low byte
ssptr_h		= FAC1_r		; string start pointer high byte

sdescr		= FAC_sc		; string descriptor pointer

csidx:		.res 1	; line crunch save index
Asptl		= csidx			; array size/pointer low byte
Aspth:		.res 1	; array size/pointer high byte

Btmpl		= Asptl			; BASIC pointer temp low byte
Btmph		= Aspth			; BASIC pointer temp low byte

Cptrl		= Asptl			; BASIC pointer temp low byte
Cptrh		= Aspth			; BASIC pointer temp low byte

Sendl		= Asptl			; BASIC pointer temp low byte
Sendh		= Aspth			; BASIC pointer temp low byte

LAB_IGBY:	.res 6	; get next BASIC byte subroutine

LAB_GBYT:	.res 1	; get current BASIC byte subroutine
Bpntrl:		.res 1	; BASIC execute (get byte) pointer low byte
Bpntrh:		.res 20	; BASIC execute (get byte) pointer high byte

;			= USER_ZP+139	; end of get BASIC char subroutine

Rbyte4:		.res 1	; extra PRNG byte
Rbyte1:		.res 1	; most significant PRNG byte
Rbyte2:		.res 1	; middle PRNG byte
Rbyte3:		.res 1	; least significant PRNG byte

NmiBase:		.res 1	; NMI handler enabled/setup/triggered flags
							; bit	function
							; ===	========
							; 7	interrupt enabled
							; 6	interrupt setup
							; 5	interrupt happened
;			= USER_ZP+145	; NMI handler addr low byte
            .res 1
;			= USER_ZP+146	; NMI handler addr high byte
            .res 1
IrqBase:		.res 1	; IRQ handler enabled/setup/triggered flags
;			= USER_ZP+148	; IRQ handler addr low byte
            .res 1
;			= USER_ZP+149	; IRQ handler addr high byte
            .res 1

tmp1:        .res 2   ; 2 bytes
ptr1 :       .res 2   ; 2 bytes

Decss:		.res 1	; number to decimal string start
Decssp1:		.res 1	; number to decimal string start

