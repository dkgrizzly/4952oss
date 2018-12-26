include "lib/header.asm"
include "lib/strap.asm"

	org 02071h
	seek 00871h
_splash_screen_data:
	DB 0ffh

	DB 003h, 00Dh, _scrattr_ascii_n
	DB "BASIC-80", 000h

	DB 006h, 009h, _scrattr_ascii_n
	DB "for the HP 4952", 000h
	DB 007h, 008h, _scrattr_ascii_n
	DB "Protocol Analyzer", 000h

	DB 00ah, 008h, _scrattr_ascii_n, 0abh
	DB " Copyright 1978", 000h
	DB 00bh, 00ch, _scrattr_ascii_n
	DB "Microsoft", 000h

	DB 000h							;; End of Screen Data

_splash_menu_data:
	DB "Re-!BERT!Remote!Mass !Run !Self~"
	DB "set!Menu!&Print!Store!Lang!Test|"

_p_main_menu_page_one:
	DW 08336h			;; First Page Menu Data
_p_mm_autoconfig:
	DW 0141ch			;; Ordinal 120h Auto Config
_p_mm_setup:
	DW 0b5a8h			;; Entry Point for Setup
_p_mm_mon:
	DW 0100dh			;; Entry Point for Monitor Menu
_p_mm_sim:
	DW 01013h			;; Entry Point for Sim Menu
_p_mm_run:
	DW 0b9ffh			;; Entry Point for Run Menu
_p_mm_exam:
	DW 013cdh			;; Ordinal 12eh Examine Data
_p_mm_next1:
	DW _p_main_menu_page_two	;; Next Page

_p_main_menu_page_two:
	DW _splash_menu_data		;; Second Page Menu Data
_p_mm_reset:
	DW 0bb1ah			;; Entry Point for Re-Set
_p_mm_bert:
	DW 0b22ch			;; Entry Point for BERT Menu
_p_mm_remote:
	DW 0d963h			;; Entry Point for Remote & Print
_p_mm_masstorage:
	DW 00f0ch			;; Entry Point for Mass Storage
_p_mm_launch_app:
	DW _launch_app			;; Entry Point for Application
_p_mm_selftest:
	DW 0136fh			;; Ordinal 12ah Self Test
_p_mm_next2:
	DW _p_main_menu_page_one	;; Next Page

_launch_app:
	ld a, 006h
	call 00e60h			; Page in 6 - Really 2 (Application "ROM" in RAM)
	ld hl,0aa00H			; Copy application to Work RAM
	ld de,_code_start		;
	ld bc,_code_end-_code_start	;
	ldir				;
	jp _app_main			; Run the application

	DW _code_end
;; End of menu section
_splash_end:

;; Main Application
;;	org 2160h
	seek 0a00h
_code_start:

_app_main:
	call _clear_screen

	ld a, 001h
	ld (_cursor_enabled), a

_main_loop:
	di
	ld a,00eh			; access desired memory bank
	out (020h),a			;

	ld SP, 0f000h
	jp COLD

_return_to_rom:
	ld a, 000h
	ld (_cursor_enabled), a
	call _clear_screen

	ld hl,_splash_screen_data	;
	push hl				;
	call 01cf8h			; Patched to 2d50 -> 01cf8h

	jp 014d5h				; Return to main menu.

include "lib/screen.asm"
include "lib/keyb.asm"


;==================================================================================
; The updates to the original BASIC within this file are copyright Grant Searle
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================

; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA

; GENERAL EQUATES

CTRLC:	equ	03H		; Control "C"
CTRLG:	equ	07H		; Control "G"
BKSP:	equ	08H		; Back space
LF:	equ	0AH		; Line feed
CS:	equ	0CH		; Clear screen
CR:	equ	0DH		; Carriage return
CTRLO:	equ	0FH		; Control "O"
CTRLQ:	equ	11H		; Control "Q"
CTRLR:	equ	12H		; Control "R"
CTRLS:	equ	13H		; Control "S"
CTRLU:	equ	15H		; Control "U"
CTRLX:	equ	18H		; Control "X"
ESC:	equ	1BH		; Escape
DEL:	equ	7FH		; Delete

; BASIC WORK SPACE LOCATIONS

BASICSTARTED:	equ	08000h

WRKSPC:	equ	08001H		; BASIC Work space
USR:	equ	WRKSPC+3H	; "USR (x)" jump
OUTSUB:	equ	WRKSPC+6H	; "OUT p,n"
OTPORT:	equ	WRKSPC+7H	; PORt (p)
DIVSUP:	equ	WRKSPC+9H	; Division support routine
DIV1:	equ	WRKSPC+0AH	; <- Values
DIV2:	equ	WRKSPC+0EH	; <-   to
DIV3:	equ	WRKSPC+12H	; <-   be
DIV4:	equ	WRKSPC+15H	; <-inserted
SEED:	equ	WRKSPC+17H	; Random number seed
LSTRND:	equ	WRKSPC+3AH	; Last random number
INPSUB:	equ	WRKSPC+3EH	; #INP (x)" Routine
INPORT:	equ	WRKSPC+3FH	; PORT (x)
NULLS:	equ	WRKSPC+41H	; Number of nulls
LWIDTH:	equ	WRKSPC+42H	; Terminal width
COMMAN:	equ	WRKSPC+43H	; Width for commas
NULFLG:	equ	WRKSPC+44H	; Null after input DB flag
CTLOFG:	equ	WRKSPC+45H	; Control "O" flag
LINESC:	equ	WRKSPC+46H	; Lines counter
LINESN:	equ	WRKSPC+48H	; Lines number
CHKSUM:	equ	WRKSPC+4AH	; Array load/save check sum
NMIFLG:	equ	WRKSPC+4CH	; Flag for NMI break routine
BRKFLG:	equ	WRKSPC+4DH	; Break flag
RINPUT:	equ	WRKSPC+4EH	; Input reflection
DCATLG:	equ	WRKSPC+51H	; Disk Catalog reflection
DLOAD:	equ	WRKSPC+54H	; Disk Load reflection
DSAVE:	equ	WRKSPC+57H	; Disk Save reflection
STRSPC:	equ	WRKSPC+5AH	; Bottom of string space
LINEAT:	equ	WRKSPC+5CH	; Current line number
BASTXT:	equ	WRKSPC+5EH	; Pointer to start of program
BUFFER:	equ	WRKSPC+61H	; Input buffer
STACK:	equ	WRKSPC+66H	; Initial stack
CURPOS:	equ	WRKSPC+0ABH	; Character position on line
LCRFLG:	equ	WRKSPC+0ACH	; Locate/Create flag
TYPE:	equ	WRKSPC+0ADH	; Data type flag
DATFLG:	equ	WRKSPC+0AEH	; Literal statement flag
LSTRAM:	equ	WRKSPC+0AFH	; Last available RAM
TMSTPT:	equ	WRKSPC+0B1H	; Temporary string pointer
TMSTPL:	equ	WRKSPC+0B3H	; Temporary string pool
TMPSTR:	equ	WRKSPC+0BFH	; Temporary string
STRBOT:	equ	WRKSPC+0C3H	; Bottom of string space
CUROPR:	equ	WRKSPC+0C5H	; Current operator in EVAL
LOOPST:	equ	WRKSPC+0C7H	; First statement of loop
DATLIN:	equ	WRKSPC+0C9H	; Line of current DATA item
FORFLG:	equ	WRKSPC+0CBH	; "FOR" loop flag
LSTBIN:	equ	WRKSPC+0CCH	; Last DB entered
READFG:	equ	WRKSPC+0CDH	; Read/Input flag
BRKLIN:	equ	WRKSPC+0CEH	; Line of break
NXTOPR:	equ	WRKSPC+0D0H	; Next operator in EVAL
ERRLIN:	equ	WRKSPC+0D2H	; Line of error
CONTAD:	equ	WRKSPC+0D4H	; Where to CONTinue
PROGND:	equ	WRKSPC+0D6H	; End of program
VAREND:	equ	WRKSPC+0D8H	; End of variables
ARREND:	equ	WRKSPC+0DAH	; End of arrays
NXTDAT:	equ	WRKSPC+0DCH	; Next data item
FNRGNM:	equ	WRKSPC+0DEH	; Name of FN argument
FNARG:	equ	WRKSPC+0E0H	; FN argument value
FPREG:	equ	WRKSPC+0E4H	; Floating point register
FPEXP:	equ	FPREG+3		; Floating point exponent
SGNRES:	equ	WRKSPC+0E8H     ; Sign of result
PBUFF:	equ	WRKSPC+0E9H     ; Number print buffer
MULVAL:	equ	WRKSPC+0F6H     ; Multiplier
PROGST:	equ	WRKSPC+0F9H     ; Start of program text area
STLOOK:	equ	WRKSPC+15DH     ; Start of memory test

; BASIC ERROR CODE VALUES

NF:	equ	00H		; NEXT without FOR
SN:	equ	02H		; Syntax error
RG:	equ	04H		; RETURN without GOSUB
OD:	equ	06H		; Out of DATA
FC:	equ	08H		; Function call error
OV:	equ	0AH		; Overflow
OM:	equ	0CH		; Out of memory
UL:	equ	0EH		; Undefined line number
BS:	equ	10H		; Bad subscript
DD:	equ	12H		; Re-DIMensioned array
DZ:	equ	14H		; Division by zero (/0)
ID:	equ	16H		; Illegal direct
TM:	equ	18H		; Type miss-match
OS:	equ	1AH		; Out of string space
LS:	equ	1CH		; String too long
ST:	equ	1EH		; String formula too complex
CN:	equ	20H		; Can't CONTinue
UF:	equ	22H		; UnDEFined FN function
MO:	equ	24H		; Missing operand
HX:	equ	26H		; HEX error
BN:	equ	28H		; BIN error

COLD:   jp      STARTA          ; Jump for cold start
WARM:   jp      WARMST          ; Jump for warm start
;STARTA:
STARTB: 
	ld      IX,0            ; Flag cold start
	jp      CSTART          ; Jump to initialise

	DW   DEINT           ; Get integer -32768 to 32767
	DW   ABPASS          ; Return integer in AB

STARTA:
	ld	A,(BASICSTARTED); Check the BASIC STARTED flag
	cp	'Y'		; to see if this is power-up
	jr	Z, WARM
;	jr	NZ,COLDSTART	; If not BASIC started then always do cold start
;CORW:
;	ld	HL,CLDMSG	; Cold/warm message
;	call    PRS          	; Output string if extra given
;	call    PROMPT          ; Get input with '?'
;	call    GETCHR          ; Get next character
;	AND	%11011111	; lower to uppercase
;	cp	'C'
;	jr	Z, COLDSTART
;	cp	'W'
;	jr	Z, WARM
;	jr	CORW
;COLDSTART:
	ld	A,'Y'		; Set the BASIC STARTED flag
	ld	(BASICSTARTED),A
	jr	STARTB		; Start BASIC COLD
;CHECKWARM:


INIT:   ld      DE,INITAB       ; Initialise workspace
	ld      B,INITBE-INITAB+3; DBs to copy
	ld      HL,WRKSPC       ; Into workspace RAM
COPY:   ld      A,(DE)          ; Get source
	ld      (HL),A          ; To destination
	inc     HL              ; Next destination
	inc     DE              ; Next source
	dec     B               ; Count DBs
	jr      NZ,COPY         ; More to move
	ld      SP,HL           ; Temporary stack
	call    CLREG           ; Clear registers and stack
	call    PRNTCRLF        ; Output CRLF
	ld      (BUFFER+72+1),A ; Mark end of buffer
	ld      (PROGST),A      ; Initialise program area
	ld      HL,00000h       ; Try to use all RAM between
	jr      SETTOP          ; Bootloader & Basic (52KB or so)
MSIZE:  ld      HL,MEMMSG       ; Point to message
	call    PRS             ; Output "Memory size"
	call    PROMPT          ; Get input with '?'
	call    GETCHR          ; Get next character
	or      A               ; Set flags
	jr      NZ,TSTMEM       ; If number - Test if RAM there
	ld      HL,STLOOK       ; Point to start of RAM
MLOOP:  inc     HL              ; Next DB
	ld      A,H             ; Above address FFFF ?
	or      L
	jr      Z,SETTOP        ; Yes - 64K RAM
	ld      A,(HL)          ; Get contents
	ld      B,A             ; Save it
	cpl                     ; Flip all bits
	ld      (HL),A          ; Put it back
	cp      (HL)            ; RAM there if same
	ld      (HL),B          ; Restore old contents
	jr      Z,MLOOP         ; If RAM - test next DB
	jr      SETTOP          ; Top of RAM found

TSTMEM: call    ATOH            ; Get high memory into DE
	or      A               ; Set flags on last DB
	jp      NZ,SNERR        ; ?SN Error if bad character
	ex      DE,HL           ; Address into HL
	dec     HL              ; Back one DB
	ld      A,11011001B     ; Test DB
	ld      B,(HL)          ; Get old contents
	ld      (HL),A          ; Load test DB
	cp      (HL)            ; RAM there if same
	ld      (HL),B          ; Restore old contents
	jr      NZ,MSIZE        ; Ask again if no RAM

SETTOP: dec     HL              ; Back one DB
	ld      DE,STLOOK-1     ; See if enough RAM
	call    CPDEHL          ; Compare DE with HL
	jp      C,MSIZE         ; Ask again if not enough RAM
	ld      DE,0-50         ; 50 DBs string space
	ld      (LSTRAM),HL     ; Save last available RAM
	add     HL,DE           ; Allocate string space
	ld      (STRSPC),HL     ; Save string space
	call    CLRPTR          ; Clear program area
	ld      HL,(STRSPC)     ; Get end of memory
	ld      DE,0-17         ; Offset for free DBs
	add     HL,DE           ; Adjust HL
	ld      DE,PROGST       ; Start of program text
	ld      A,L             ; Get LSB
	sub     E               ; Adjust it
	ld      L,A             ; Re-save
	ld      A,H             ; Get MSB
	sbc     A,D             ; Adjust it
	ld      H,A             ; Re-save
	push    HL              ; Save bytes free
	ld      HL,SIGNON       ; Sign-on message
	call    PRS             ; Output string
	pop     HL              ; Get bytes free back
	call    PRNTHL          ; Output amount of free memory
	ld      HL,BFREE        ; " bytes free" message
	call    PRS             ; Output string

WARMST: ld      SP,STACK        ; Temporary stack
BRKRET: call    CLREG           ; Clear registers and stack
	jp      PRNTOK          ; Go to get command line

BFREE:  DB   " bytes free",CR,LF,0,0

SIGNON: DB   "Z80 BASIC Ver 4.7b",CR,LF
	DB   "Copyright ",0abh
	DB   " 1978 by Microsoft",CR,LF,0,0

MEMMSG: DB   "Memory top",0
;CLDMSG:	DB	"Cold or warm start (C or W)",0

; FUNCTION addRESS TABLE

FNCTAB: DW   SGN
	DW   INT
	DW   ABS
	DW   USR
	DW   FRE
	DW   INP
	DW   POS
	DW   SQR
	DW   RND
	DW   LOG
	DW   EXP
	DW   COS
	DW   SIN
	DW   TAN
	DW   ATN
	DW   PEEK
	DW   DEEK
	DW   POINT
	DW   LEN
	DW   STR
	DW   VAL
	DW   ASC
	DW   CHR
	DW   HEX
	DW   BIN
	DW   LEFT
	DW   RIGHT
	DW   MID

; RESERVED DW LIST

DWS:  DB   ('E'+80H),"ND"
	DB   ('F'+80H),"OR"
	DB   ('N'+80H),"EXT"
	DB   ('D'+80H),"ATA"
	DB   ('I'+80H),"NPUT"
	DB   ('D'+80H),"IM"
	DB   ('R'+80H),"EAD"
	DB   ('L'+80H),"ET"
	DB   ('G'+80H),"OTO"
	DB   ('R'+80H),"UN"
	DB   ('I'+80H),"F"
	DB   ('R'+80H),"ESTORE"
	DB   ('G'+80H),"OSUB"
	DB   ('R'+80H),"ETURN"
	DB   ('R'+80H),"EM"
	DB   ('S'+80H),"TOP"
	DB   ('O'+80H),"UT"
	DB   ('O'+80H),"N"
	DB   ('N'+80H),"ULL"
	DB   ('W'+80H),"AIT"
	DB   ('D'+80H),"EF"
	DB   ('P'+80H),"OKE"
	DB   ('D'+80H),"OKE"
	DB   ('S'+80H),"CREEN"
	DB   ('L'+80H),"INES"
	DB   ('C'+80H),"LS"
	DB   ('W'+80H),"IDTH"
	DB   ('C'+80H),"ALL"
	DB   ('C'+80H),"AT"
	DB   ('R'+80H),"ESET"
	DB   ('P'+80H),"RINT"
	DB   ('C'+80H),"ONT"
	DB   ('L'+80H),"IST"
	DB   ('C'+80H),"LEAR"
	DB   ('D'+80H),"LOAD"
	DB   ('D'+80H),"SAVE"
	DB   ('N'+80H),"EW"

	DB   ('T'+80H),"AB("
	DB   ('T'+80H),"O"
	DB   ('F'+80H),"N"
	DB   ('S'+80H),"PC("
	DB   ('T'+80H),"HEN"
	DB   ('N'+80H),"OT"
	DB   ('S'+80H),"TEP"

	DB   ('+'+80H)
	DB   ('-'+80H)
	DB   ('*'+80H)
	DB   ('/'+80H)
	DB   ('^'+80H)
	DB   ('A'+80H),"ND"
	DB   ('O'+80H),"R"
	DB   ('>'+80H)
	DB   ('='+80H)
	DB   ('<'+80H)

	DB   ('S'+80H),"GN"
	DB   ('I'+80H),"NT"
	DB   ('A'+80H),"BS"
	DB   ('U'+80H),"SR"
	DB   ('F'+80H),"RE"
	DB   ('I'+80H),"NP"
	DB   ('P'+80H),"OS"
	DB   ('S'+80H),"QR"
	DB   ('R'+80H),"ND"
	DB   ('L'+80H),"OG"
	DB   ('E'+80H),"XP"
	DB   ('C'+80H),"OS"
	DB   ('S'+80H),"IN"
	DB   ('T'+80H),"AN"
	DB   ('A'+80H),"TN"
	DB   ('P'+80H),"EEK"
	DB   ('D'+80H),"EEK"
	DB   ('P'+80H),"OINT"
	DB   ('L'+80H),"EN"
	DB   ('S'+80H),"TR$"
	DB   ('V'+80H),"AL"
	DB   ('A'+80H),"SC"
	DB   ('C'+80H),"HR$"
	DB   ('H'+80H),"ex$"
	DB   ('B'+80H),"IN$"
	DB   ('L'+80H),"EFT$"
	DB   ('R'+80H),"IGHT$"
	DB   ('M'+80H),"ID$"
	DB   80H             ; End of list marker

; KEYDW addRESS TABLE

DWTB: DW   PEND
	DW   FOR
	DW   NEXT
	DW   DATA
	DW   INPUT
	DW   DIM
	DW   READ
	DW   LET
	DW   GOTO
	DW   RUN
	DW   IF
	DW   RESTOR
	DW   GOSUB
	DW   RETURN
	DW   REM
	DW   STOP
	DW   POUT
	DW   ON
	DW   NULL
	DW   WAIT
	DW   DEF
	DW   POKE
	DW   DOKE
	DW   REM
	DW   LINES
	DW   CLS
	DW   WIDTH
	DW   C_A_L_L
	DW   DCATLG
	DW   MONITR
	DW   PRINT
	DW   CONT
	DW   LIST
	DW   CLEAR
	DW   DLOAD
	DW   DSAVE
	DW   NEW

; RESERVED DW TOKEN VALUES

ZEND:	equ	080H            ; END
ZFOR:	equ	081H            ; FOR
ZDATA:	equ	083H            ; DATA
ZGOTO:	equ	088H            ; GOTO
ZGOSUB:	equ	08CH            ; GOSUB
ZREM:	equ	08EH            ; REM
ZPRINT:	equ	09EH            ; PRINT
ZNEW:	equ	0A4H            ; NEW

ZTAB:	equ	0A5H            ; TAB
ZTO:	equ	0A6H            ; TO
ZFN:	equ	0A7H            ; FN
ZSPC:	equ	0A8H            ; SPC
ZTHEN:	equ	0A9H            ; THEN
ZNOT:	equ	0AAH            ; NOT
ZSTEP:	equ	0ABH            ; STEP

ZPLUS:	equ	0ACH            ; +
ZMINUS:	equ	0ADH            ; -
ZTIMES:	equ	0AEH            ; *
ZDIV:	equ	0AFH            ; /
ZOR:	equ	0B2H            ; OR
ZGTR:	equ	0B3H            ; >
ZEQUAL:	equ	0B4H            ; M
ZLTH:	equ	0B5H            ; <
ZSGN:	equ	0B6H            ; SGN
ZPOINT:	equ	0C7H            ; POINT
ZLEFT:	equ	0CDH +2         ; LEFT$

; ARITHMETIC PRECEDENCE TABLE

PRITAB: DB   79H             ; Precedence value
	DW   PADD            ; FPREG = <last> + FPREG

	DB   79H             ; Precedence value
	DW   PSUB            ; FPREG = <last> - FPREG

	DB   7CH             ; Precedence value
	DW   MULT            ; PPREG = <last> * FPREG

	DB   7CH             ; Precedence value
	DW   DIV             ; FPREG = <last> / FPREG

	DB   7FH             ; Precedence value
	DW   POWER           ; FPREG = <last> ^ FPREG

	DB   50H             ; Precedence value
	DW   PAND            ; FPREG = <last> and FPREG

	DB   46H             ; Precedence value
	DW   POR             ; FPREG = <last> or FPREG

; BASIC ERROR CODE LIST

ERRORS: DB   "NF"            ; NEXT without FOR
	DB   "SN"            ; Syntax error
	DB   "RG"            ; RETURN without GOSUB
	DB   "OD"            ; Out of DATA
	DB   "FC"            ; Illegal function call
	DB   "OV"            ; Overflow error
	DB   "OM"            ; Out of memory
	DB   "UL"            ; Undefined line
	DB   "BS"            ; Bad subscript
	DB   "DD"            ; Re-DIMensioned array
	DB   "/0"            ; Division by zero
	DB   "ID"            ; Illegal direct
	DB   "TM"            ; Type mis-match
	DB   "OS"            ; Out of string space
	DB   "LS"            ; String too long
	DB   "ST"            ; String formula too complex
	DB   "CN"            ; Can't CONTinue
	DB   "UF"            ; Undefined FN function
	DB   "MO"            ; Missing operand
	DB   "HX"            ; HEX error
	DB   "BN"            ; BIN error

; INITIALISATION TABLE -------------------------------------------------------

INITAB: jp      WARMST          ; Warm start jump
	jp      FCERR           ; "USR (X)" jump (Set to Error)
	OUT     (0),A           ; "OUT p,n" skeleton
	ret
	sub     0               ; Division support routine
	ld      L,A
	ld      A,H
	sbc     A,0
	ld      H,A
	ld      A,B
	sbc     A,0
	ld      B,A
	ld      A,0
	ret
	DB   0,0,0                   ; Random number seed table used by RND
	DB   035H,04AH,0CAH,099H     ;-2.65145E+07
	DB   039H,01CH,076H,098H     ; 1.61291E+07
	DB   022H,095H,0B3H,098H     ;-1.17691E+07
	DB   00AH,0DDH,047H,098H     ; 1.30983E+07
	DB   053H,0D1H,099H,099H     ;-2-01612E+07
	DB   00AH,01AH,09FH,098H     ;-1.04269E+07
	DB   065H,0BCH,0CDH,098H     ;-1.34831E+07
	DB   0D6H,077H,03EH,098H     ; 1.24825E+07
	DB   052H,0C7H,04FH,080H     ; Last random number
	IN      A,(0)           ; INP (x) skeleton
	ret
	DB   1               ; POS (x) number (1)
	DB   255             ; Terminal width (255 = no auto CRLF)
	DB   28              ; Width for commas (3 columns)
	DB   0               ; No nulls after input DBs
	DB   0               ; Output enabled (^O off)
	DW   20              ; Initial lines counter
	DW   20              ; Initial lines number
	DW   0               ; Array load/save check sum
	DB   0               ; Break not by NMI
	DB   0               ; Break flag
	jp      TTYLIN          ; Input reflection (set to TTY)
	jp      REM             ; Disk Catalog reflection unused
	jp      REM             ; Disk Load reflection
	jp      REM          	; Disk Save reflection
	DW   STLOOK          ; Temp string space
	DW   -2              ; Current line number (cold)
	DW   PROGST+1        ; Start of program text
INITBE:                         

; END OF INITIALISATION TABLE ---------------------------------------------------

ERRMSG: DB   " Error",0
INMSG:  DB   " in ",0
ZERBYT:	equ    $-1             ; A zero DB
OKMSG:  DB   "Ok",CR,LF,0,0
BRKMSG: DB   "Break",0

BAKSTK: ld      HL,4            ; Look for "FOR" block with
	add     HL,SP           ; same index as specified
LOKFOR: ld      A,(HL)          ; Get block ID
	inc     HL              ; Point to index address
	cp      ZFOR            ; Is it a "FOR" token
	ret     NZ              ; No - exit
	ld      C,(HL)          ; BC = Address of "FOR" index
	inc     HL
	ld      B,(HL)
	inc     HL              ; Point to sign of STEP
	push    HL              ; Save pointer to sign
	ld      L,C             ; HL = address of "FOR" index
	ld      H,B
	ld      A,D             ; See if an index was specified
	or      E               ; DE = 0 if no index specified
	ex      DE,HL           ; Specified index into HL
	jr      Z,INDFND        ; Skip if no index given
	ex      DE,HL           ; Index back into DE
	call    CPDEHL          ; Compare index with one given
INDFND: ld      BC,16-3         ; Offset to next block
	pop     HL              ; Restore pointer to sign
	ret     Z               ; Return if block found
	add     HL,BC           ; Point to next block
	jr      LOKFOR          ; Keep on looking

MOVUP:  call    ENFMEM          ; See if enough memory
MOVSTR: push    BC              ; Save end of source
	ex      (SP),HL         ; Swap source and dest" end
	pop     BC              ; Get end of destination
MOVLP:  call    CPDEHL          ; See if list moved
	ld      A,(HL)          ; Get DB
	ld      (BC),A          ; Move it
	ret     Z               ; Exit if all done
	dec     BC              ; Next DB to move to
	dec     HL              ; Next DB to move
	jp      MOVLP           ; Loop until all DBs moved

CHKSTK: push    HL              ; Save code string address
	ld      HL,(ARREND)     ; Lowest free memory
	ld      B,0             ; BC = Number of levels to test
	add     HL,BC           ; 2 DBs for each level
	add     HL,BC
	DB   3EH             ; Skip "push HL"
ENFMEM: push    HL              ; Save code string address
	ld      A,0D0H ;LOW -48 ; 48 DBs minimum RAM
	sub     L
	ld      L,A
	ld      A,0FFH; HIGH (-48) ; 48 DBs minimum RAM
	sbc     A,H
	jp      C,OMERR         ; Not enough - ?OM Error
	ld      H,A
	add     HL,SP           ; Test if stack is overflowed
	pop     HL              ; Restore code string address
	ret     C               ; Return if enough mmory
OMERR:  ld      E,OM            ; ?OM Error
	jr      ERROR

DATSNR: ld      HL,(DATLIN)     ; Get line of current DATA item
	ld      (LINEAT),HL     ; Save as current line
SNERR:  ld      E,SN            ; ?SN Error
	DB   01H             ; Skip "ld E,DZ"
DZERR:  ld      E,DZ            ; ?/0 Error
	DB   01H             ; Skip "ld E,NF"
NFERR:  ld      E,NF            ; ?NF Error
	DB   01H             ; Skip "ld E,DD"
DDERR:  ld      E,DD            ; ?DD Error
	DB   01H             ; Skip "ld E,UF"
UFERR:  ld      E,UF            ; ?UF Error
	DB   01H             ; Skip "ld E,OV
OVERR:  ld      E,OV            ; ?OV Error
	DB   01H             ; Skip "ld E,TM"
TMERR:  ld      E,TM            ; ?TM Error

ERROR:  call    CLREG           ; Clear registers and stack
	ld      (CTLOFG),A      ; Enable output (A is 0)
	call    STTLIN          ; Start new line
	ld      HL,ERRORS       ; Point to error codes
	ld      D,A             ; D = 0 (A is 0)
	ld      A,'?'
	call    OUTC            ; Output '?'
	add     HL,DE           ; Offset to correct error code
	ld      A,(HL)          ; First character
	call    OUTC            ; Output it
	call    GETCHR          ; Get next character
	call    OUTC            ; Output it
	ld      HL,ERRMSG       ; "Error" message
ERRIN:  call    PRS             ; Output message
	ld      HL,(LINEAT)     ; Get line of error
	ld      DE,-2           ; Cold start error if -2
	call    CPDEHL          ; See if cold start error
	jp      Z,CSTART        ; Cold start error - Restart
	ld      A,H             ; Was it a direct error?
	and     L               ; Line = -1 if direct error
	inc     A
	call    NZ,LINEIN       ; No - output line of error
	DB   3EH             ; Skip "pop BC"
POPNOK: pop     BC              ; Drop address in input buffer

PRNTOK: xor     A               ; Output "Ok" and get command
	ld      (CTLOFG),A      ; Enable output
	call    STTLIN          ; Start new line
	ld      HL,OKMSG        ; "Ok" message
	call    PRS             ; Output "Ok"
GETCMD: ld      HL,-1           ; Flag direct mode
	ld      (LINEAT),HL     ; Save as current line
	call    GETLIN          ; Get an input line
	jp      C,GETCMD        ; Get line again if break
	call    GETCHR          ; Get first character
	inc     A               ; Test if end of line
	dec     A               ; Without affecting Carry
	jr      Z,GETCMD        ; Nothing entered - Get another
	push    AF              ; Save Carry status
	call    ATOH            ; Get line number into DE
	push    DE              ; Save line number
	call    CRUNCH	; Tokenise rest of line
	ld      B,A             ; Length of tokenised line
	pop     DE              ; Restore line number
	pop     AF              ; Restore Carry
	jp      NC,EXCUTE       ; No line number - Direct mode
	push    DE              ; Save line number
	push    BC              ; Save length of tokenised line
	xor      A
	ld      (LSTBIN),A      ; Clear last DB input
	call    GETCHR          ; Get next character
	or      A               ; Set flags
	push    AF              ; And save them
	call    SRCHLN          ; Search for line number in DE
	jr      C,LINFND        ; Jump if line found
	pop     AF              ; Get status
	push    AF              ; And re-save
	jp      Z,ULERR         ; Nothing after number - Error
	or      A               ; Clear Carry
LINFND: push    BC              ; Save address of line in prog
	jp      NC,INEWLN       ; Line not found - Insert new
	ex      DE,HL           ; Next line address in DE
	ld      HL,(PROGND)     ; End of program
SFTPRG: ld      A,(DE)          ; Shift rest of program down
	ld      (BC),A
	inc     BC              ; Next destination
	inc     DE              ; Next source
	call    CPDEHL          ; All done?
	jr      NZ,SFTPRG       ; More to do
	ld      H,B             ; HL - New end of program
	ld      L,C
	ld      (PROGND),HL     ; Update end of program

INEWLN: pop     DE              ; Get address of line,
	pop     AF              ; Get status
	jp      Z,SETPTR        ; No text - Set up pointers
	ld      HL,(PROGND)     ; Get end of program
	ex      (SP),HL         ; Get length of input line
	pop     BC              ; End of program to BC
	add     HL,BC           ; Find new end
	push    HL              ; Save new end
	call    MOVUP           ; Make space for line
	pop     HL              ; Restore new end
	ld      (PROGND),HL     ; Update end of program pointer
	ex      DE,HL           ; Get line to move up in HL
	ld      (HL),H	; Save MSB
	pop     DE              ; Get new line number
	inc     HL              ; Skip pointer
	inc     HL
	ld      (HL),E          ; Save LSB of line number
	inc     HL
	ld      (HL),D          ; Save MSB of line number
	inc     HL              ; To first DB in line
	ld      DE,BUFFER       ; Copy buffer to program
MOVBUF: ld      A,(DE)          ; Get source
	ld      (HL),A          ; Save destinations
	inc     HL              ; Next source
	inc     DE              ; Next destination
	or      A               ; Done?
	jr      NZ,MOVBUF       ; No - Repeat
SETPTR: call    RUNFST          ; Set line pointers
	inc     HL              ; To LSB of pointer
	ex      DE,HL           ; Address to DE
PTRLP:  ld      H,D             ; Address to HL
	ld      L,E
	ld      A,(HL)          ; Get LSB of pointer
	inc     HL              ; To MSB of pointer
	or      (HL)            ; Compare with MSB pointer
	jp      Z,GETCMD        ; Get command line if end
	inc     HL              ; To LSB of line number
	inc     HL              ; Skip line number
	inc     HL              ; Point to first DB in line
	xor      A               ; Looking for 00 DB
FNDEND: cp      (HL)            ; Found end of line?
	inc     HL              ; Move to next DB
	jp      NZ,FNDEND       ; No - Keep looking
	ex      DE,HL           ; Next line address to HL
	ld      (HL),E          ; Save LSB of pointer
	inc     HL
	ld      (HL),D          ; Save MSB of pointer
	jp      PTRLP           ; Do next line

SRCHLN: ld      HL,(BASTXT)     ; Start of program text
SRCHLP: ld      B,H             ; BC = Address to look at
	ld      C,L
	ld      A,(HL)          ; Get address of next line
	inc     HL
	or      (HL)            ; End of program found?
	dec     HL
	ret     Z               ; Yes - Line not found
	inc     HL
	inc     HL
	ld      A,(HL)          ; Get LSB of line number
	inc     HL
	ld      H,(HL)          ; Get MSB of line number
	ld      L,A
	call    CPDEHL          ; Compare with line in DE
	ld      H,B             ; HL = Start of this line
	ld      L,C
	ld      A,(HL)          ; Get LSB of next line address
	inc     HL
	ld      H,(HL)          ; Get MSB of next line address
	ld      L,A             ; Next line to HL
	CCF
	ret     Z               ; Lines found - Exit
	CCF
	ret     NC              ; Line not found,at line after
	jr      SRCHLP          ; Keep looking

NEW:    ret     NZ              ; Return if any more on line
CLRPTR: ld      HL,(BASTXT)     ; Point to start of program
	xor      A               ; Set program area to empty
	ld      (HL),A          ; Save LSB = 00
	inc     HL
	ld      (HL),A          ; Save MSB = 00
	inc     HL
	ld      (PROGND),HL     ; Set program end

RUNFST: ld      HL,(BASTXT)     ; Clear all variables
	dec     HL

INTVAR: ld      (BRKLIN),HL     ; Initialise RUN variables
	ld      HL,(LSTRAM)     ; Get end of RAM
	ld      (STRBOT),HL     ; Clear string space
	xor      A
	call    RESTOR          ; Reset DATA pointers
	ld      HL,(PROGND)     ; Get end of program
	ld      (VAREND),HL     ; Clear variables
	ld      (ARREND),HL     ; Clear arrays

CLREG:  pop     BC              ; Save return address
	ld      HL,(STRSPC)     ; Get end of working RAN
	ld      SP,HL           ; Set stack
	ld      HL,TMSTPL       ; Temporary string pool
	ld      (TMSTPT),HL     ; Reset temporary string ptr
	xor      A               ; A = 00
	ld      L,A             ; HL = 0000
	ld      H,A
	ld      (CONTAD),HL     ; No CONTinue
	ld      (FORFLG),A      ; Clear FOR flag
	ld      (FNRGNM),HL     ; Clear FN argument
	push    HL              ; HL = 0000
	push    BC              ; Put back return
DOAGN:  ld      HL,(BRKLIN)     ; Get address of code to RUN
	ret                     ; Return to execution driver

PROMPT: ld      A,'?'           ; '?'
	call    OUTC            ; Output character
	ld      A,' '           ; Space
	call    OUTC            ; Output character
	jp      RINPUT          ; Get input line

CRUNCH: xor     A               ; Tokenise line @ HL to BUFFER
	ld      (DATFLG),A      ; Reset literal flag
	ld      C,2+3           ; 2 DB number and 3 nulls
	ld      DE,BUFFER       ; Start of input buffer
CRNCLP: ld      A,(HL)          ; Get DB
	cp      ' '             ; Is it a space?
	jp      Z,MOVDIR        ; Yes - Copy direct
	ld      B,A             ; Save character
	cp      '"'             ; Is it a quote?
	jp      Z,CPYLIT        ; Yes - Copy literal string
	or      A               ; Is it end of buffer?
	jp      Z,ENDBUF        ; Yes - End buffer
	ld      A,(DATFLG)      ; Get data type
	or      A               ; Literal?
	ld      A,(HL)          ; Get DB to copy
	jp      NZ,MOVDIR       ; Literal - Copy direct
	cp      '?'             ; Is it '?' short for PRINT
	ld      A,ZPRINT        ; "PRINT" token
	jp      Z,MOVDIR        ; Yes - replace it
	ld      A,(HL)          ; Get DB again
	cp      '0'             ; Is it less than '0'
	jr      C,FNDWRD        ; Yes - Look for reserved DWs
	cp      60; ";"+1           ; Is it "0123456789:;" ?
	jp      C,MOVDIR        ; Yes - copy it direct
FNDWRD: push    DE              ; Look for reserved DWs
	ld      DE,DWS-1      ; Point to table
	push    BC              ; Save count
	ld      BC,RETNAD       ; Where to return to
	push    BC              ; Save return address
	ld      B,ZEND-1        ; First token value -1
	ld      A,(HL)          ; Get DB
	cp      'a'             ; Less than 'a' ?
	jr      C,SEARCH        ; Yes - search for DWs
	cp      'z'+1           ; Greater than 'z' ?
	jr      NC,SEARCH       ; Yes - search for DWs
	and     01011111B       ; FORce upper case
	ld      (HL),A          ; Replace DB
SEARCH: ld      C,(HL)          ; Search for a DW
	ex      DE,HL
GETNXT: inc     HL              ; Get next reserved DW
	or      (HL)            ; Start of DW?
	jp      P,GETNXT        ; No - move on
	inc     B               ; Increment token value
	ld      A, (HL)         ; Get DB from table
	and     01111111B       ; Strip bit 7
	ret     Z               ; Return if end of list
	cp      C               ; Same character as in buffer?
	jr      NZ,GETNXT       ; No - get next DW
	ex      DE,HL
	push    HL              ; Save start of DW

NXTBYT: inc     DE              ; Look through rest of DW
	ld      A,(DE)          ; Get DB from table
	or      A               ; End of DW ?
	jp      M,MATCH         ; Yes - Match found
	ld      C,A             ; Save it
	ld      A,B             ; Get token value
	cp      ZGOTO           ; Is it "GOTO" token ?
	jr      NZ,NOSPC        ; No - Don't allow spaces
	call    GETCHR          ; Get next character
	dec     HL              ; Cancel increment from GETCHR
NOSPC:  inc     HL              ; Next DB
	ld      A,(HL)          ; Get DB
	cp      'a'             ; Less than 'a' ?
	jr      C,NOCHNG        ; Yes - don't change
	and     01011111B       ; Make upper case
NOCHNG: cp      C               ; Same as in buffer ?
	jp      Z,NXTBYT        ; Yes - keep testing
	pop     HL              ; Get back start of DW
	jp      SEARCH	; Look at next DW

MATCH:  ld      C,B             ; DW found - Save token value
	pop     AF              ; Throw away return
	ex      DE,HL
	ret                     ; Return to "RETNAD"
RETNAD: ex      DE,HL           ; Get address in string
	ld      A,C             ; Get token value
	pop     BC              ; Restore buffer length
	pop     DE              ; Get destination address
MOVDIR: inc     HL              ; Next source in buffer
	ld      (DE),A          ; Put DB in buffer
	inc     DE              ; Move up buffer
	inc     C               ; Increment length of buffer
	sub     ':'             ; End of statement?
	jr      Z,SETLIT        ; Jump if multi-statement line
	cp      ZDATA-3AH       ; Is it DATA statement ?
	jr      NZ,TSTREM       ; No - see if REM
SETLIT: ld      (DATFLG),A      ; Set literal flag
TSTREM: sub     ZREM-3AH        ; Is it REM?
	jp      NZ,CRNCLP       ; No - Leave flag
	ld      B,A             ; Copy rest of buffer
NXTCHR: ld      A,(HL)          ; Get DB
	or      A               ; End of line ?
	jr      Z,ENDBUF        ; Yes - Terminate buffer
	cp      B               ; End of statement ?
	jr      Z,MOVDIR        ; Yes - Get next one
CPYLIT: inc     HL              ; Move up source string
	ld      (DE),A          ; Save in destination
	inc     C               ; Increment length
	inc     DE              ; Move up destination
	jr      NXTCHR          ; Repeat

ENDBUF: ld      HL,BUFFER-1     ; Point to start of buffer
	ld      (DE),A          ; Mark end of buffer (A = 00)
	inc     DE
	ld      (DE),A          ; A = 00
	inc     DE
	ld      (DE),A          ; A = 00
	ret

DODEL:  ld      A,(NULFLG)      ; Get null flag status
	or      A               ; Is it zero?
	ld      A,0             ; Zero A - Leave flags
	ld      (NULFLG),A      ; Zero null flag
	jr      NZ,ECHDEL       ; Set - Echo it
	dec     B               ; Decrement length
	jr      Z,GETLIN        ; Get line again if empty
	call    OUTC            ; Output null character
	DB   3EH             ; Skip "dec B"
ECHDEL: dec     B               ; Count DBs in buffer
	dec     HL              ; Back space buffer
	jr      Z,OTKLN         ; No buffer - Try again
	ld      A,(HL)          ; Get deleted DB
	call    OUTC            ; Echo it
	jp      MORINP          ; Get more input

DELCHR: dec     B               ; Count DBs in buffer
	dec     HL              ; Back space buffer
	call    OUTC            ; Output character in A
	jp      NZ,MORINP       ; Not end - Get more
OTKLN:  call    OUTC            ; Output character in A
KILIN:  call    PRNTCRLF        ; Output CRLF
	jr      TTYLIN          ; Get line again

GETLIN:
TTYLIN: ld      HL,BUFFER       ; Get a line by character
	ld      B,1             ; Set buffer as empty
	xor      A
	ld      (NULFLG),A      ; Clear null flag
MORINP: call    CLOTST          ; Get character and test ^O
	ld      C,A             ; Save character in C
	cp      DEL             ; Delete character?
	jp      Z,DODEL         ; Yes - Process it
	ld      A,(NULFLG)      ; Get null flag
	or      A               ; Test null flag status
	jp      Z,PROCES        ; Reset - Process character
	ld      A,0             ; Set a null
	call    OUTC            ; Output null
	xor      A               ; Clear A
	ld      (NULFLG),A      ; Reset null flag
PROCES: ld      A,C             ; Get character
	cp      CTRLG           ; Bell?
	jp      Z,PUTCTL        ; Yes - Save it
	cp      CTRLC           ; Is it control "C"?
	call    Z,PRNTCRLF      ; Yes - Output CRLF
	SCF                     ; Flag break
	ret     Z               ; Return if control "C"
	cp      CR              ; Is it enter?
	jp      Z,ENDINP        ; Yes - Terminate input
	cp      CTRLU           ; Is it control "U"?
	jp      Z,KILIN         ; Yes - Get another line
	cp      CTRLX           ; Is it "kill line"?
	jp      Z,OTKLN         ; Yes - Kill line
	cp      DEL             ; Is it delete?
	jr      Z,DELCHR        ; Yes - Delete character
	cp      BKSP            ; Is it backspace?
	jr      Z,DELCHR        ; Yes - Delete character
	cp	80h		; Is it > 0x7f?
	jr	NC,MORINP	; Skip it then
	cp      CTRLR           ; Is it control "R"?
	jr      NZ,PUTBUF       ; No - Put in buffer
	push    BC              ; Save buffer length
	push    DE              ; Save DE
	push    HL              ; Save buffer address
	ld      (HL),0          ; Mark end of buffer
	call    OUTNCR          ; Output and do CRLF
	ld      HL,BUFFER       ; Point to buffer start
	call    PRS             ; Output buffer
	pop     HL              ; Restore buffer address
	pop     DE              ; Restore DE
	pop     BC              ; Restore buffer length
	jp      MORINP          ; Get another character

PUTBUF: cp      ' '             ; Is it a control code?
	jr      C,MORINP        ; Yes - Ignore
PUTCTL: ld      A,B             ; Get number of DBs in buffer
	cp      72+1            ; Test for line overflow
	ld      A,CTRLG         ; Set a bell
	jr      NC,OUTNBS       ; Ring bell if buffer full
	ld      A,C             ; Get character
	ld      (HL),C          ; Save in buffer
	ld      (LSTBIN),A      ; Save last input DB
	inc     HL              ; Move up buffer
	inc     B               ; Increment length
OUTIT:  call    OUTC            ; Output the character entered
	jp      MORINP          ; Get another character

OUTNBS: call    OUTC            ; Output bell and back over it
	ld      A,BKSP          ; Set back space
	jr      OUTIT           ; Output it and get more

CPDEHL: ld      A,H             ; Get H
	sub     D               ; Compare with D
	ret     NZ              ; Different - Exit
	ld      A,L             ; Get L
	sub     E               ; Compare with E
	ret                     ; Return status

CHKSYN: ld      A,(HL)          ; Check syntax of character
	ex      (SP),HL         ; Address of test DB
	cp      (HL)            ; Same as in code string?
	inc     HL              ; Return address
	ex      (SP),HL         ; Put it back
	jp      Z,GETCHR        ; Yes - Get next character
	jp      SNERR           ; Different - ?SN Error

OUTC:   push    AF              ; Save character
	ld      A,(CTLOFG)      ; Get control "O" flag
	or      A               ; Is it set?
	jp      NZ,POPAF        ; Yes - don't output
	pop     AF              ; Restore character
	push    BC              ; Save buffer length
	push    AF              ; Save character
	cp      ' '             ; Is it a control code?
	jr      C,DINPOS        ; Yes - Don't inc POS(X)
	ld      A,(LWIDTH)      ; Get line width
	ld      B,A             ; To B
	ld      A,(CURPOS)      ; Get cursor position
	inc     B               ; Width 255?
	jr      Z,INCLEN        ; Yes - No width limit
	dec     B               ; Restore width
	cp      B               ; At end of line?
	call    Z,PRNTCRLF      ; Yes - output CRLF
INCLEN: inc     A               ; Move on one character
	ld      (CURPOS),A      ; Save new position
DINPOS: pop     AF              ; Restore character
	pop     BC              ; Restore buffer length
	call    MONOUT          ; Send it
	ret

CLOTST: call    GETINP          ; Get input character
	cp	_key_enter
	jr	z,_fix_enter
	cp	_key_lt
	jr	z,_fix_lt
;        and     01111111B       ; Strip bit 7
	cp      CTRLO           ; Is it control "O"?
	ret     NZ              ; No don't flip flag
	ld      A,(CTLOFG)      ; Get flag
	cpl                     ; Flip it
	ld      (CTLOFG),A      ; Put it back
	xor      A               ; Null character
	ret
_fix_lt:
	ld	A, BKSP
	ret
_fix_enter:
	ld	A, CR
	ret

LIST:   call    ATOH            ; ASCII number to DE
	ret     NZ              ; Return if anything extra
	pop     BC              ; Rubbish - Not needed
	call    SRCHLN          ; Search for line number in DE
	push    BC              ; Save address of line
	call    SETLIN          ; Set up lines counter
LISTLP: pop     HL              ; Restore address of line
	ld      C,(HL)          ; Get LSB of next line
	inc     HL
	ld      B,(HL)          ; Get MSB of next line
	inc     HL
	ld      A,B             ; BC = 0 (End of program)?
	or      C
	jp      Z,PRNTOK        ; Yes - Go to command mode
	call    COUNT           ; Count lines
	call    TSTBRK          ; Test for break key
	push    BC              ; Save address of next line
	call    PRNTCRLF        ; Output CRLF
	ld      E,(HL)          ; Get LSB of line number
	inc     HL
	ld      D,(HL)          ; Get MSB of line number
	inc     HL
	push    HL              ; Save address of line start
	ex      DE,HL           ; Line number to HL
	call    PRNTHL          ; Output line number in decimal
	ld      A,' '           ; Space after line number
	pop     HL              ; Restore start of line address
LSTLP2: call    OUTC            ; Output character in A
LSTLP3: ld      A,(HL)          ; Get next DB in line
	or      A               ; End of line?
	inc     HL              ; To next DB in line
	jr      Z,LISTLP        ; Yes - get next line
	jp      P,LSTLP2        ; No token - output it
	sub     ZEND-1          ; Find and output DW
	ld      C,A             ; Token offset+1 to C
	ld      DE,DWS        ; Reserved DW list
FNDTOK: ld      A,(DE)          ; Get character in list
	inc     DE              ; Move on to next
	or      A               ; Is it start of DW?
	jp      P,FNDTOK        ; No - Keep looking for DW
	dec     C               ; Count DWs
	jr      NZ,FNDTOK       ; Not there - keep looking
OUTWRD: and     01111111B       ; Strip bit 7
	call    OUTC            ; Output first character
	ld      A,(DE)          ; Get next character
	inc     DE              ; Move on to next
	or      A               ; Is it end of DW?
	jp      P,OUTWRD        ; No - output the rest
	jr      LSTLP3          ; Next DB in line

SETLIN: push    HL              ; Set up LINES counter
	ld      HL,(LINESN)     ; Get LINES number
	ld      (LINESC),HL     ; Save in LINES counter
	pop     HL
	ret

COUNT:  push    HL              ; Save code string address
	push    DE
	ld      HL,(LINESC)     ; Get LINES counter
	ld      DE,-1
	adc     HL,DE           ; Decrement
	ld      (LINESC),HL     ; Put it back
	pop     DE
	pop     HL              ; Restore code string address
	ret     P               ; Return if more lines to go
	push    HL              ; Save code string address
	ld      HL,(LINESN)     ; Get LINES number
	ld      (LINESC),HL     ; Reset LINES counter
	call    GETINP          ; Get input character
	cp      CTRLC           ; Is it control "C"?
	jr      Z,RSLNBK        ; Yes - Reset LINES and break
	pop     HL              ; Restore code string address
	jr      COUNT           ; Keep on counting

RSLNBK: ld      HL,(LINESN)     ; Get LINES number
	ld      (LINESC),HL     ; Reset LINES counter
	jp      BRKRET          ; Go and output "Break"

FOR:    ld      A,64H	; Flag "FOR" assignment
	ld      (FORFLG),A      ; Save "FOR" flag
	call    LET             ; Set up initial index
	pop     BC              ; Drop return address
	push    HL              ; Save code string address
	call    DATA            ; Get next statement address
	ld      (LOOPST),HL     ; Save it for start of loop
	ld      HL,2            ; Offset for "FOR" block
	add     HL,SP           ; Point to it
FORSLP: call    LOKFOR          ; Look for existing "FOR" block
	pop     DE              ; Get code string address
	jp      NZ,FORFND       ; No nesting found
	add     HL,BC           ; Move into "FOR" block
	push    DE              ; Save code string address
	dec     HL
	ld      D,(HL)          ; Get MSB of loop statement
	dec     HL
	ld      E,(HL)          ; Get LSB of loop statement
	inc     HL
	inc     HL
	push    HL              ; Save block address
	ld      HL,(LOOPST)     ; Get address of loop statement
	call    CPDEHL          ; Compare the FOR loops
	pop     HL              ; Restore block address
	jr      NZ,FORSLP       ; Different FORs - Find another
	pop     DE              ; Restore code string address
	ld      SP,HL           ; Remove all nested loops

FORFND: ex      DE,HL           ; Code string address to HL
	ld      C,8
	call    CHKSTK          ; Check for 8 levels of stack
	push    HL              ; Save code string address
	ld      HL,(LOOPST)     ; Get first statement of loop
	ex      (SP),HL         ; Save and restore code string
	push    HL              ; Re-save code string address
	ld      HL,(LINEAT)     ; Get current line number
	ex      (SP),HL         ; Save and restore code string
	call    TSTNUM          ; Make sure it's a number
	call    CHKSYN          ; Make sure "TO" is next
	DB   ZTO          ; "TO" token
	call    GETNUM          ; Get "TO" expression value
	push    HL              ; Save code string address
	call    BCDEFP          ; Move "TO" value to BCDE
	pop     HL              ; Restore code string address
	push    BC              ; Save "TO" value in block
	push    DE
	ld      BC,8100H        ; BCDE - 1 (default STEP)
	ld      D,C             ; C=0
	ld      E,D             ; D=0
	ld      A,(HL)          ; Get next DB in code string
	cp      ZSTEP           ; See if "STEP" is stated
	ld      A,1             ; Sign of step = 1
	jr      NZ,SAVSTP       ; No STEP given - Default to 1
	call    GETCHR          ; Jump over "STEP" token
	call    GETNUM          ; Get step value
	push    HL              ; Save code string address
	call    BCDEFP          ; Move STEP to BCDE
	call    TSTSGN          ; Test sign of FPREG
	pop     HL              ; Restore code string address
SAVSTP: push    BC              ; Save the STEP value in block
	push    DE
	push    AF              ; Save sign of STEP
	inc     SP              ; Don't save flags
	push    HL              ; Save code string address
	ld      HL,(BRKLIN)     ; Get address of index variable
	ex      (SP),HL         ; Save and restore code string
PUTFID: ld      B,ZFOR          ; "FOR" block marker
	push    BC              ; Save it
	inc     SP              ; Don't save C

RUNCNT: call    TSTBRK          ; Execution driver - Test break
	ld      (BRKLIN),HL     ; Save code address for break
	ld      A,(HL)          ; Get next DB in code string
	cp      ':'             ; Multi statement line?
	jr      Z,EXCUTE        ; Yes - Execute it
	or      A               ; End of line?
	jp      NZ,SNERR        ; No - Syntax error
	inc     HL              ; Point to address of next line
	ld      A,(HL)          ; Get LSB of line pointer
	inc     HL
	or      (HL)            ; Is it zero (End of prog)?
	jp      Z,ENDPRG        ; Yes - Terminate execution
	inc     HL              ; Point to line number
	ld      E,(HL)          ; Get LSB of line number
	inc     HL
	ld      D,(HL)          ; Get MSB of line number
	ex      DE,HL           ; Line number to HL
	ld      (LINEAT),HL     ; Save as current line number
	ex      DE,HL           ; Line number back to DE
EXCUTE: call    GETCHR          ; Get key DW
	ld      DE,RUNCNT       ; Where to return to
	push    DE              ; Save for return
IFJMP:  ret     Z               ; Go to RUNCNT if end of STMT
ONJMP:  sub     ZEND            ; Is it a token?
	jp      C,LET           ; No - try to assign it
	cp      ZNEW+1-ZEND     ; END to NEW ?
	jp      NC,SNERR        ; Not a key DW - ?SN Error
	rlca                    ; Double it
	ld      C,A             ; BC = Offset into table
	ld      B,0
	ex      DE,HL           ; Save code string address
	ld      HL,DWTB       ; KeyDW address table
	add     HL,BC           ; Point to routine address
	ld      C,(HL)          ; Get LSB of routine address
	inc     HL
	ld      B,(HL)          ; Get MSB of routine address
	push    BC              ; Save routine address
	ex      DE,HL           ; Restore code string address

GETCHR: inc     HL              ; Point to next character
	ld      A,(HL)          ; Get next code string DB
	cp      ':'             ; Z if ':'
	ret     NC              ; NC if > "9"
	cp      ' '
	jr      Z,GETCHR        ; Skip over spaces
	cp      '0'
	CCF                     ; NC if < '0'
	inc     A               ; Test for zero - Leave carry
	dec     A               ; Z if Null
	ret

RESTOR: ex      DE,HL           ; Save code string address
	ld      HL,(BASTXT)     ; Point to start of program
	jr      Z,RESTNL        ; Just RESTORE - reset pointer
	ex      DE,HL           ; Restore code string address
	call    ATOH            ; Get line number to DE
	push    HL              ; Save code string address
	call    SRCHLN          ; Search for line number in DE
	ld      H,B             ; HL = Address of line
	ld      L,C
	pop     DE              ; Restore code string address
	jp      NC,ULERR        ; ?UL Error if not found
RESTNL: dec     HL              ; DB before DATA statement
UPDATA: ld      (NXTDAT),HL     ; Update DATA pointer
	ex      DE,HL           ; Restore code string address
	ret


TSTBRK: 
	call _keyscan
	call _getkey_nowait
	cp	0ffh
	ret     Z               ; No key, go back
	cp      _key_exit       ; Exit Key
	jr      Z,BRK           ; Yes, break
	cp      003h            ; <Ctrl-C>
	jr      Z,BRK           ; Yes, break
	cp      _key_more       ; Stop scrolling?
	ret     NZ              ; Other key, ignore


STALL:
	call _keyscan
	call _getkey_nowait
	cp      _key_more       ; Resume scrolling?
	ret      Z              ; Release the chokehold
	cp      003h            ; Second break?
	jr      Z,STOP          ; Break during hold exits prog
	cp      _key_exit       ; Second break?
	jr      Z,STOP          ; Break during hold exits prog
	jr      STALL           ; Loop until <Ctrl-Q> or <brk>

BRK:
	ld      A,$FF           ; Set BRKFLG
	ld      (BRKFLG),A      ; Store it


STOP:   ret     NZ              ; Exit if anything else
	DB   0F6H            ; Flag "STOP"
PEND:   ret     NZ              ; Exit if anything else
	ld      (BRKLIN),HL     ; Save point of break
	DB   21H             ; Skip "or 11111111B"
INPBRK: or      11111111B       ; Flag "Break" wanted
	pop     BC              ; Return not needed and more
ENDPRG: ld      HL,(LINEAT)     ; Get current line number
	push    AF              ; Save STOP / END status
	ld      A,L             ; Is it direct break?
	and     H
	inc     A               ; Line is -1 if direct break
	jr      Z,NOLIN         ; Yes - No line number
	ld      (ERRLIN),HL     ; Save line of break
	ld      HL,(BRKLIN)     ; Get point of break
	ld      (CONTAD),HL     ; Save point to CONTinue
NOLIN:  xor      A
	ld      (CTLOFG),A      ; Enable output
	call    STTLIN          ; Start a new line
	pop     AF              ; Restore STOP / END status
	ld      HL,BRKMSG       ; "Break" message
	jp      NZ,ERRIN        ; "in line" wanted?
	jp      PRNTOK          ; Go to command mode

CONT:   ld      HL,(CONTAD)     ; Get CONTinue address
	ld      A,H             ; Is it zero?
	or      L
	ld      E,CN            ; ?CN Error
	jp      Z,ERROR         ; Yes - output "?CN Error"
	ex      DE,HL           ; Save code string address
	ld      HL,(ERRLIN)     ; Get line of last break
	ld      (LINEAT),HL     ; Set up current line number
	ex      DE,HL           ; Restore code string address
	ret                     ; CONTinue where left off

NULL:   call    GETINT          ; Get integer 0-255
	ret     NZ              ; Return if bad value
	ld      (NULLS),A       ; Set nulls number
	ret


ACCSUM: push    HL              ; Save address in array
	ld      HL,(CHKSUM)     ; Get check sum
	ld      B,0             ; BC - Value of DB
	ld      C,A
	add     HL,BC           ; Add DB to check sum
	ld      (CHKSUM),HL     ; Re-save check sum
	pop     HL              ; Restore address in array
	ret

CHKLTR: ld      A,(HL)          ; Get DB
	cp      'A'             ; < 'a' ?
	ret     C               ; Carry set if not letter
	cp      'Z'+1           ; > 'z' ?
	CCF
	ret                     ; Carry set if not letter

FPSINT: call    GETCHR          ; Get next character
POSINT: call    GETNUM          ; Get integer 0 to 32767
DEPINT: call    TSTSGN          ; Test sign of FPREG
	jp      M,FCERR         ; Negative - ?FC Error
DEINT:  ld      A,(FPEXP)       ; Get integer value to DE
	cp      80H+16          ; Exponent in range (16 bits)?
	jp      C,FPINT         ; Yes - convert it
	ld      BC,9080H        ; BCDE = -32768
	ld      DE,0000
	push    HL              ; Save code string address
	call    CMPNUM          ; Compare FPREG with BCDE
	pop     HL              ; Restore code string address
	ld      D,C             ; MSB to D
	ret     Z               ; Return if in range
FCERR:  ld      E,FC            ; ?FC Error
	jp      ERROR           ; Output error-

ATOH:   dec     HL              ; ASCII number to DE binary
GETLN:  ld      DE,0            ; Get number to DE
GTLNLP: call    GETCHR          ; Get next character
	ret     NC              ; Exit if not a digit
	push    HL              ; Save code string address
	push    AF              ; Save digit
	ld      HL,65529/10     ; Largest number 65529
	call    CPDEHL          ; Number in range?
	jp      C,SNERR         ; No - ?SN Error
	ld      H,D             ; HL = Number
	ld      L,E
	add     HL,DE           ; Times 2
	add     HL,HL           ; Times 4
	add     HL,DE           ; Times 5
	add     HL,HL           ; Times 10
	pop     AF              ; Restore digit
	sub     '0'             ; Make it 0 to 9
	ld      E,A             ; DE = Value of digit
	ld      D,0
	add     HL,DE           ; Add to number
	ex      DE,HL           ; Number to DE
	pop     HL              ; Restore code string address
	jr      GTLNLP          ; Go to next character

CLEAR:  jp      Z,INTVAR        ; Just "CLEAR" Keep parameters
	call    POSINT          ; Get integer 0 to 32767 to DE
	dec     HL              ; Cancel increment
	call    GETCHR          ; Get next character
	push    HL              ; Save code string address
	ld      HL,(LSTRAM)     ; Get end of RAM
	jp      Z,STORED        ; No value given - Use stored
	pop     HL              ; Restore code string address
	call    CHKSYN          ; Check for comma
	DB      ','
	push    DE              ; Save number
	call    POSINT          ; Get integer 0 to 32767
	dec     HL              ; Cancel increment
	call    GETCHR          ; Get next character
	jp      NZ,SNERR        ; ?SN Error if more on line
	ex      (SP),HL         ; Save code string address
	ex      DE,HL           ; Number to DE
STORED: ld      A,L             ; Get LSB of new RAM top
	sub     E               ; Subtract LSB of string space
	ld      E,A             ; Save LSB
	ld      A,H             ; Get MSB of new RAM top
	sbc     A,D             ; Subtract MSB of string space
	ld      D,A             ; Save MSB
	jp      C,OMERR         ; ?OM Error if not enough mem
	push    HL              ; Save RAM top
	ld      HL,(PROGND)     ; Get program end
	ld      BC,40           ; 40 DBs minimum working RAM
	add     HL,BC           ; Get lowest address
	call    CPDEHL          ; Enough memory?
	jp      NC,OMERR        ; No - ?OM Error
	ex      DE,HL           ; RAM top to HL
	ld      (STRSPC),HL     ; Set new string space
	pop     HL              ; End of memory to use
	ld      (LSTRAM),HL     ; Set new top of RAM
	pop     HL              ; Restore code string address
	jp      INTVAR          ; Initialise variables

RUN:    jp      Z,RUNFST        ; RUN from start if just RUN
	call    INTVAR          ; Initialise variables
	ld      BC,RUNCNT       ; Execution driver loop
	jr      RUNLIN          ; RUN from line number

GOSUB:  ld      C,3             ; 3 Levels of stack needed
	call    CHKSTK          ; Check for 3 levels of stack
	pop     BC              ; Get return address
	push    HL              ; Save code string for RETURN
	push    HL              ; And for GOSUB routine
	ld      HL,(LINEAT)     ; Get current line
	ex      (SP),HL         ; Into stack - Code string out
	ld      A,ZGOSUB        ; "GOSUB" token
	push    AF              ; Save token
	inc     SP              ; Don't save flags

RUNLIN: push    BC              ; Save return address
GOTO:   call    ATOH            ; ASCII number to DE binary
	call    REM             ; Get end of line
	push    HL              ; Save end of line
	ld      HL,(LINEAT)     ; Get current line
	call    CPDEHL          ; Line after current?
	pop     HL              ; Restore end of line
	inc     HL              ; Start of next line
	call    C,SRCHLP        ; Line is after current line
	call    NC,SRCHLN       ; Line is before current line
	ld      H,B             ; Set up code string address
	ld      L,C
	dec     HL              ; Incremented after
	ret     C               ; Line found
ULERR:  ld      E,UL            ; ?UL Error
	jp      ERROR           ; Output error message

RETURN: ret     NZ              ; Return if not just RETURN
	ld      D,-1            ; Flag "GOSUB" search
	call    BAKSTK          ; Look "GOSUB" block
	ld      SP,HL           ; Kill all FORs in subroutine
	cp      ZGOSUB          ; Test for "GOSUB" token
	ld      E,RG            ; ?RG Error
	jp      NZ,ERROR        ; Error if no "GOSUB" found
	pop     HL              ; Get RETURN line number
	ld      (LINEAT),HL     ; Save as current
	inc     HL              ; Was it from direct statement?
	ld      A,H
	or      L               ; Return to line
	jr      NZ,RETLIN       ; No - Return to line
	ld      A,(LSTBIN)      ; Any INPUT in subroutine?
	or      A               ; If so buffer is corrupted
	jp      NZ,POPNOK       ; Yes - Go to command mode
RETLIN: ld      HL,RUNCNT       ; Execution driver loop
	ex      (SP),HL         ; Into stack - Code string out
	DB      3EH             ; Skip "pop HL"
NXTDTA: pop     HL              ; Restore code string address

DATA:   DB      01H,3AH         ; ':' End of statement
REM:    ld      C,0             ; 00  End of statement
	ld      B,0
NXTSTL: ld      A,C             ; Statement and DB
	ld      C,B
	ld      B,A             ; Statement end DB
NXTSTT: ld      A,(HL)          ; Get DB
	or      A               ; End of line?
	ret     Z               ; Yes - Exit
	cp      B               ; End of statement?
	ret     Z               ; Yes - Exit
	inc     HL              ; Next DB
	cp      '"'             ; Literal string?
	jr      Z,NXTSTL        ; Yes - Look for another '"'
	jr      NXTSTT          ; Keep looking

LET:    call    GETVAR          ; Get variable name
	call    CHKSYN          ; Make sure "=" follows
	DB      ZEQUAL          ; "=" token
	push    DE              ; Save address of variable
	ld      A,(TYPE)        ; Get data type
	push    AF              ; Save type
	call    EVAL            ; Evaluate expression
	pop     AF              ; Restore type
	ex      (SP),HL         ; Save code - Get var addr
	ld      (BRKLIN),HL     ; Save address of variable
	rra                     ; Adjust type
	call    CHKTYP          ; Check types are the same
	jr      Z,LETNUM        ; Numeric - Move value
LETSTR: push    HL              ; Save address of string var
	ld      HL,(FPREG)      ; Pointer to string entry
	push    HL              ; Save it on stack
	inc     HL              ; Skip over length
	inc     HL
	ld      E,(HL)          ; LSB of string address
	inc     HL
	ld      D,(HL)          ; MSB of string address
	ld      HL,(BASTXT)     ; Point to start of program
	call    CPDEHL          ; Is string before program?
	jr      NC,CRESTR       ; Yes - Create string entry
	ld      HL,(STRSPC)     ; Point to string space
	call    CPDEHL          ; Is string literal in program?
	pop     DE              ; Restore address of string
	jp      NC,MVSTPT       ; Yes - Set up pointer
	ld      HL,TMPSTR       ; Temporary string pool
	call    CPDEHL          ; Is string in temporary pool?
	jr      NC,MVSTPT       ; No - Set up pointer
	DB   3EH             ; Skip "pop DE"
CRESTR: pop     DE              ; Restore address of string
	call    BAKTMP          ; Back to last tmp-str entry
	ex      DE,HL           ; Address of string entry
	call    SAVSTR          ; Save string in string area
MVSTPT: call    BAKTMP          ; Back to last tmp-str entry
	pop     HL              ; Get string pointer
	call    DETHL4          ; Move string pointer to var
	pop     HL              ; Restore code string address
	ret

LETNUM: push    HL              ; Save address of variable
	call    FPTHL           ; Move value to variable
	pop     DE              ; Restore address of variable
	pop     HL              ; Restore code string address
	ret

ON:     call    GETINT          ; Get integer 0-255
	ld      A,(HL)          ; Get "GOTO" or "GOSUB" token
	ld      B,A             ; Save in B
	cp      ZGOSUB          ; "GOSUB" token?
	jr      Z,ONGO          ; Yes - Find line number
	call    CHKSYN          ; Make sure it's "GOTO"
	DB   ZGOTO           ; "GOTO" token
	dec     HL              ; Cancel increment
ONGO:   ld      C,E             ; Integer of branch value
ONGOLP: dec     C               ; Count branches
	ld      A,B             ; Get "GOTO" or "GOSUB" token
	jp      Z,ONJMP         ; Go to that line if right one
	call    GETLN           ; Get line number to DE
	cp      ','             ; Another line number?
	ret     NZ              ; No - Drop through
	jp      ONGOLP          ; Yes - loop

IF:     call    EVAL            ; Evaluate expression
	ld      A,(HL)          ; Get token
	cp      ZGOTO           ; "GOTO" token?
	jr      Z,IFGO          ; Yes - Get line
	call    CHKSYN          ; Make sure it's "THEN"
	DB      ZTHEN           ; "THEN" token
	dec     HL              ; Cancel increment
IFGO:   call    TSTNUM          ; Make sure it's numeric
	call    TSTSGN          ; Test state of expression
	jp      Z,REM           ; False - Drop through
	call    GETCHR          ; Get next character
	jp      C,GOTO          ; Number - GOTO that line
	jp      IFJMP           ; Otherwise do statement

MRPRNT: dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
PRINT:  jp      Z,PRNTCRLF      ; CRLF if just PRINT
PRNTLP: ret     Z               ; End of list - Exit
	cp      ZTAB            ; "TAB(" token?
	jp      Z,DOTAB         ; Yes - Do TAB routine
	cp      ZSPC            ; "SPC(" token?
	jp      Z,DOTAB         ; Yes - Do SPC routine
	push    HL              ; Save code string address
	cp      ','             ; Comma?
	jp      Z,DOCOM         ; Yes - Move to next zone
	cp      59 ;";"         ; Semi-colon?
	jp      Z,NEXITM        ; Do semi-colon routine
	pop     BC              ; Code string address to BC
	call    EVAL            ; Evaluate expression
	push    HL              ; Save code string address
	ld      A,(TYPE)        ; Get variable type
	or      A               ; Is it a string variable?
	jp      NZ,PRNTST       ; Yes - Output string contents
	call    NUMASC          ; Convert number to text
	call    CRTST           ; Create temporary string
	ld      (HL),' '        ; Followed by a space
	ld      HL,(FPREG)      ; Get length of output
	inc     (HL)            ; Plus 1 for the space
	ld      HL,(FPREG)      ; < Not needed >
	ld      A,(LWIDTH)      ; Get width of line
	ld      B,A             ; To B
	inc     B               ; Width 255 (No limit)?
	jp      Z,PRNTNB        ; Yes - Output number string
	inc     B               ; Adjust it
	ld      A,(CURPOS)      ; Get cursor position
	add     A,(HL)          ; Add length of string
	dec     A               ; Adjust it
	cp      B               ; Will output fit on this line?
	call    NC,PRNTCRLF     ; No - CRLF first
PRNTNB: call    PRS1            ; Output string at (HL)
	xor      A               ; Skip call by setting 'z' flag
PRNTST: call    NZ,PRS1         ; Output string at (HL)
	pop     HL              ; Restore code string address
	jp      MRPRNT          ; See if more to PRINT

STTLIN: ld      A,(CURPOS)      ; Make sure on new line
	or      A               ; Already at start?
	ret     Z               ; Yes - Do nothing
	jp      PRNTCRLF        ; Start a new line

ENDINP: ld      (HL),0          ; Mark end of buffer
	ld      HL,BUFFER-1     ; Point to buffer
PRNTCRLF: ld    A,CR            ; Load a CR
	call    OUTC            ; Output character
	ld      A,LF            ; Load a LF
	call    OUTC            ; Output character
DONULL: xor      A               ; Set to position 0
	ld      (CURPOS),A      ; Store it
	ld      A,(NULLS)       ; Get number of nulls
NULLP:  dec     A               ; Count them
	ret     Z               ; Return if done
	push    AF              ; Save count
	xor      A               ; Load a null
	call    OUTC            ; Output it
	pop     AF              ; Restore count
	jr      NULLP           ; Keep counting

DOCOM:  ld      A,(COMMAN)      ; Get comma width
	ld      B,A             ; Save in B
	ld      A,(CURPOS)      ; Get current position
	cp      B               ; Within the limit?
	call    NC,PRNTCRLF     ; No - output CRLF
	jp      NC,NEXITM       ; Get next item
ZONELP: sub     14              ; Next zone of 14 characters
	jr      NC,ZONELP       ; Repeat if more zones
	cpl                     ; Number of spaces to output
	jr      ASPCS           ; Output them

DOTAB:  push    AF              ; Save token
	call    FNDNUM          ; Evaluate expression
	call    CHKSYN          ; Make sure ")" follows
	DB   ")"
	dec     HL              ; Back space on to ")"
	pop     AF              ; Restore token
	sub     ZSPC            ; Was it "SPC(" ?
	push    HL              ; Save code string address
	jr      Z,DOSPC         ; Yes - Do 'E' spaces
	ld      A,(CURPOS)      ; Get current position
DOSPC:  cpl                     ; Number of spaces to print to
	add     A,E             ; Total number to print
	jr      NC,NEXITM       ; TAB < Current POS(X)
ASPCS:  inc     A               ; Output A spaces
	ld      B,A             ; Save number to print
	ld      A,' '           ; Space
SPCLP:  call    OUTC            ; Output character in A
	dec     B               ; Count them
	jr      NZ,SPCLP        ; Repeat if more
NEXITM: pop     HL              ; Restore code string address
	call    GETCHR          ; Get next character
	jp      PRNTLP          ; More to print

REDO:   DB   "?Redo from start",CR,LF,0

BADINP: ld      A,(READFG)      ; READ or INPUT?
	or      A
	jp      NZ,DATSNR       ; READ - ?SN Error
	pop     BC              ; Throw away code string addr
	ld      HL,REDO         ; "Redo from start" message
	call    PRS             ; Output string
	jp      DOAGN           ; Do last INPUT again

INPUT:  call    IDTEST          ; Test for illegal direct
	ld      A,(HL)          ; Get character after "INPUT"
	cp      '"'             ; Is there a prompt string?
	ld      A,0             ; Clear A and leave flags
	ld      (CTLOFG),A      ; Enable output
	jp      NZ,NOPMPT       ; No prompt - get input
	call    QTSTR           ; Get string terminated by '"'
	call    CHKSYN          ; Check for ';' after prompt
	DB   ';'
	push    HL              ; Save code string address
	call    PRS1            ; Output prompt string
	DB   3EH             ; Skip "push HL"
NOPMPT: push    HL              ; Save code string address
	call    PROMPT          ; Get input with "? " prompt
	pop     BC              ; Restore code string address
	jp      C,INPBRK        ; Break pressed - Exit
	inc     HL              ; Next DB
	ld      A,(HL)          ; Get it
	or      A               ; End of line?
	dec     HL              ; Back again
	push    BC              ; Re-save code string address
	jp      Z,NXTDTA        ; Yes - Find next DATA stmt
	ld      (HL),','        ; Store comma as separator
	jr      NXTITM          ; Get next item

READ:   push    HL              ; Save code string address
	ld      HL,(NXTDAT)     ; Next DATA statement
	DB   0F6H            ; Flag "READ"
NXTITM: xor      A               ; Flag "INPUT"
	ld      (READFG),A      ; Save "READ"/"INPUT" flag
	ex      (SP),HL         ; Get code str' , Save pointer
	jr      GTVLUS          ; Get values

NEDMOR: call    CHKSYN          ; Check for comma between items
	DB      ','
GTVLUS: call    GETVAR          ; Get variable name
	ex      (SP),HL         ; Save code str" , Get pointer
	push    DE              ; Save variable address
	ld      A,(HL)          ; Get next "INPUT"/"DATA" DB
	cp      ','             ; Comma?
	jp      Z,ANTVLU        ; Yes - Get another value
	ld      A,(READFG)      ; Is it READ?
	or      A
	jp      NZ,FDTLP        ; Yes - Find next DATA stmt
	ld      A,'?'           ; More INPUT needed
	call    OUTC            ; Output character
	call    PROMPT          ; Get INPUT with prompt
	pop     DE              ; Variable address
	pop     BC              ; Code string address
	jp      C,INPBRK        ; Break pressed
	inc     HL              ; Point to next DATA DB
	ld      A,(HL)          ; Get DB
	or      A               ; Is it zero (No input) ?
	dec     HL              ; Back space INPUT pointer
	push    BC              ; Save code string address
	jp      Z,NXTDTA        ; Find end of buffer
	push    DE              ; Save variable address
ANTVLU: ld      A,(TYPE)        ; Check data type
	or      A               ; Is it numeric?
	jr      Z,INPBIN        ; Yes - Convert to binary
	call    GETCHR          ; Get next character
	ld      D,A             ; Save input character
	ld      B,A             ; Again
	cp      '"'             ; Start of literal sting?
	jr      Z,STRENT        ; Yes - Create string entry
	ld      A,(READFG)      ; "READ" or "INPUT" ?
	or      A
	ld      D,A             ; Save 00 if "INPUT"
	jr      Z,ITMSEP        ; "INPUT" - End with 00
	ld      D,':'           ; "DATA" - End with 00 or ':'
ITMSEP: ld      B,','           ; Item separator
	dec     HL              ; Back space for DTSTR
STRENT: call    DTSTR           ; Get string terminated by D
	ex      DE,HL           ; String address to DE
	ld      HL,LTSTND       ; Where to go after LETSTR
	ex      (SP),HL         ; Save HL , get input pointer
	push    DE              ; Save address of string
	jp      LETSTR          ; Assign string to variable

INPBIN: call    GETCHR          ; Get next character
	call    ASCTFP          ; Convert ASCII to FP number
	ex      (SP),HL         ; Save input ptr, Get var addr
	call    FPTHL           ; Move FPREG to variable
	pop     HL              ; Restore input pointer
LTSTND: dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	jr      Z,MORDT         ; End of line - More needed?
	cp      ','             ; Another value?
	jp      NZ,BADINP       ; No - Bad input
MORDT:  ex      (SP),HL         ; Get code string address
	dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	jp      NZ,NEDMOR       ; More needed - Get it
	pop     DE              ; Restore DATA pointer
	ld      A,(READFG)      ; "READ" or "INPUT" ?
	or      A
	ex      DE,HL           ; DATA pointer to HL
	jp      NZ,UPDATA       ; Update DATA pointer if "READ"
	push    DE              ; Save code string address
	or      (HL)            ; More input given?
	ld      HL,EXTIG        ; "?Extra ignored" message
	call    NZ,PRS          ; Output string if extra given
	pop     HL              ; Restore code string address
	ret

EXTIG:  DB   "?Extra ignored",CR,LF,0

FDTLP:  call    DATA            ; Get next statement
	or      A               ; End of line?
	jp      NZ,FANDT        ; No - See if DATA statement
	inc     HL
	ld      A,(HL)          ; End of program?
	inc     HL
	or      (HL)            ; 00 00 Ends program
	ld      E,OD            ; ?OD Error
	jp      Z,ERROR         ; Yes - Out of DATA
	inc     HL
	ld      E,(HL)          ; LSB of line number
	inc     HL
	ld      D,(HL)          ; MSB of line number
	ex      DE,HL
	ld      (DATLIN),HL     ; Set line of current DATA item
	ex      DE,HL
FANDT:  call    GETCHR          ; Get next character
	cp      ZDATA           ; "DATA" token
	jp      NZ,FDTLP        ; No "DATA" - Keep looking
	jp      ANTVLU          ; Found - Convert input

NEXT:   ld      DE,0            ; In case no index given
NEXT1:  call    NZ,GETVAR       ; Get index address
	ld      (BRKLIN),HL     ; Save code string address
	call    BAKSTK          ; Look for "FOR" block
	jp      NZ,NFERR        ; No "FOR" - ?NF Error
	ld      SP,HL           ; Clear nested loops
	push    DE              ; Save index address
	ld      A,(HL)          ; Get sign of STEP
	inc     HL
	push    AF              ; Save sign of STEP
	push    DE              ; Save index address
	call    PHLTFP          ; Move index value to FPREG
	ex      (SP),HL         ; Save address of TO value
	push    HL              ; Save address of index
	call    ADDPHL          ; Add STEP to index value
	pop     HL              ; Restore address of index
	call    FPTHL           ; Move value to index variable
	pop     HL              ; Restore address of TO value
	call    LOADFP          ; Move TO value to BCDE
	push    HL              ; Save address of line of FOR
	call    CMPNUM          ; Compare index with TO value
	pop     HL              ; Restore address of line num
	pop     BC              ; Address of sign of STEP
	sub     B               ; Compare with expected sign
	call    LOADFP          ; BC = Loop stmt,DE = Line num
	jr      Z,KILFOR        ; Loop finished - Terminate it
	ex      DE,HL           ; Loop statement line number
	ld      (LINEAT),HL     ; Set loop line number
	ld      L,C             ; Set code string to loop
	ld      H,B
	jp      PUTFID          ; Put back "FOR" and continue

KILFOR: ld      SP,HL           ; Remove "FOR" block
	ld      HL,(BRKLIN)     ; Code string after "NEXT"
	ld      A,(HL)          ; Get next DB in code string
	cp      ','             ; More NEXTs ?
	jp      NZ,RUNCNT       ; No - Do next statement
	call    GETCHR          ; Position to index name
	call    NEXT1           ; Re-enter NEXT routine
; < will not return to here , Exit to RUNCNT or Loop >

GETNUM: call    EVAL            ; Get a numeric expression
TSTNUM: DB      0F6H            ; Clear carry (numeric)
TSTSTR: SCF                     ; Set carry (string)
CHKTYP: ld      A,(TYPE)        ; Check types match
	adc     A,A             ; Expected + actual
	or      A               ; Clear carry , set parity
	ret     PE              ; Even parity - Types match
	jp      TMERR           ; Different types - Error

OPNPAR: call    CHKSYN          ; Make sure "(" follows
	DB   "("
EVAL:   dec     HL              ; Evaluate expression & save
	ld      D,0             ; Precedence value
EVAL1:  push    DE              ; Save precedence
	ld      C,1
	call    CHKSTK          ; Check for 1 level of stack
	call    OPRND           ; Get next expression value
EVAL2:  ld      (NXTOPR),HL     ; Save address of next operator
EVAL3:  ld      HL,(NXTOPR)     ; Restore address of next opr
	pop     BC              ; Precedence value and operator
	ld      A,B             ; Get precedence value
	cp      78H             ; "AND" or "OR" ?
	call    NC,TSTNUM       ; No - Make sure it's a number
	ld      A,(HL)          ; Get next operator / function
	ld      D,0             ; Clear Last relation
RLTLP:  sub     ZGTR            ; ">" Token
	jr      C,FOPRND        ; + - * / ^ and or - Test it
	cp      ZLTH+1-ZGTR     ; < = >
	jr      NC,FOPRND       ; Function - Call it
	cp      ZEQUAL-ZGTR     ; "="
	rla                     ; <- Test for legal
	xor      D               ; <- combinations of < = >
	cp      D               ; <- by combining last token
	ld      D,A             ; <- with current one
	jp      C,SNERR         ; Error if "<<' '==" or ">>"
	ld      (CUROPR),HL     ; Save address of current token
	call    GETCHR          ; Get next character
	jr      RLTLP           ; Treat the two as one

FOPRND: ld      A,D             ; < = > found ?
	or      A
	jp      NZ,TSTRED       ; Yes - Test for reduction
	ld      A,(HL)          ; Get operator token
	ld      (CUROPR),HL     ; Save operator address
	sub     ZPLUS           ; Operator or function?
	ret     C               ; Neither - Exit
	cp      ZOR+1-ZPLUS     ; Is it + - * / ^ and or ?
	ret     NC              ; No - Exit
	ld      E,A             ; Coded operator
	ld      A,(TYPE)        ; Get data type
	dec     A               ; FF = numeric , 00 = string
	or      E               ; Combine with coded operator
	ld      A,E             ; Get coded operator
	jp      Z,CONCAT        ; String concatenation
	rlca                    ; Times 2
	add     A,E             ; Times 3
	ld      E,A             ; To DE (D is 0)
	ld      HL,PRITAB       ; Precedence table
	add     HL,DE           ; To the operator concerned
	ld      A,B             ; Last operator precedence
	ld      D,(HL)          ; Get evaluation precedence
	cp      D               ; Compare with eval precedence
	ret     NC              ; Exit if higher precedence
	inc     HL              ; Point to routine address
	call    TSTNUM          ; Make sure it's a number

STKTHS: push    BC              ; Save last precedence & token
	ld      BC,EVAL3        ; Where to go on prec' break
	push    BC              ; Save on stack for return
	ld      B,E             ; Save operator
	ld      C,D             ; Save precedence
	call    STAKFP          ; Move value to stack
	ld      E,B             ; Restore operator
	ld      D,C             ; Restore precedence
	ld      C,(HL)          ; Get LSB of routine address
	inc     HL
	ld      B,(HL)          ; Get MSB of routine address
	inc     HL
	push    BC              ; Save routine address
	ld      HL,(CUROPR)     ; Address of current operator
	jp      EVAL1           ; Loop until prec' break

OPRND:  xor      A               ; Get operand routine
	ld      (TYPE),A        ; Set numeric expected
	call    GETCHR          ; Get next character
	ld      E,MO            ; ?MO Error
	jp      Z,ERROR         ; No operand - Error
	jp      C,ASCTFP        ; Number - Get value
	call    CHKLTR          ; See if a letter
	jp      NC,CONVAR       ; Letter - Find variable
	cp	'&'		; &H = HEX, &B = BINARY
	jr	NZ, NOTAMP
	call    GETCHR          ; Get next character
	cp      'H'             ; HEX number indicated? [function added]
	jp      Z,HEXTFP        ; Convert HEX to FPREG
	cp      'B'             ; Binary number indicated? [function added]
	jp      Z,BINTFP        ; Convert Bin to FPREG
	ld      E,SN            ; If neither then a ?SN Error
	jp      Z,ERROR         ; 
NOTAMP: cp      ZPLUS           ; '+' Token ?
	jp      Z,OPRND         ; Yes - Look for operand
	cp      '.'             ; '.' ?
	jp      Z,ASCTFP        ; Yes - Create FP number
	cp      ZMINUS          ; '-' Token ?
	jr      Z,MINUS         ; Yes - Do minus
	cp      '"'             ; Literal string ?
	jp      Z,QTSTR         ; Get string terminated by '"'
	cp      ZNOT            ; "NOT" Token ?
	jp      Z,EVNOT         ; Yes - Eval NOT expression
	cp      ZFN             ; "FN" Token ?
	jp      Z,DOFN          ; Yes - Do FN routine
	sub     ZSGN            ; Is it a function?
	jp      NC,FNOFST       ; Yes - Evaluate function
EVLPAR: call    OPNPAR          ; Evaluate expression in "()"
	call    CHKSYN          ; Make sure ")" follows
	DB   ")"
	ret

MINUS:  ld      D,7DH	; '-' precedence
	call    EVAL1           ; Evaluate until prec' break
	ld      HL,(NXTOPR)     ; Get next operator address
	push    HL              ; Save next operator address
	call    INVSGN          ; Negate value
RETNUM: call    TSTNUM          ; Make sure it's a number
	pop     HL              ; Restore next operator address
	ret

CONVAR: call    GETVAR          ; Get variable address to DE
FRMEVL: push    HL              ; Save code string address
	ex      DE,HL           ; Variable address to HL
	ld      (FPREG),HL      ; Save address of variable
	ld      A,(TYPE)        ; Get type
	or      A               ; Numeric?
	call    Z,PHLTFP        ; Yes - Move contents to FPREG
	pop     HL              ; Restore code string address
	ret

FNOFST: ld      B,0             ; Get address of function
	rlca                    ; Double function offset
	ld      C,A             ; BC = Offset in function table
	push    BC              ; Save adjusted token value
	call    GETCHR          ; Get next character
	ld      A,C             ; Get adjusted token value
	cp      2*(ZLEFT-ZSGN)-1; Adj' LEFT$,RIGHT$ or MID$ ?
	jr      C,FNVAL         ; No - Do function
	call    OPNPAR          ; Evaluate expression  (X,...
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	call    TSTSTR          ; Make sure it's a string
	ex      DE,HL           ; Save code string address
	ld      HL,(FPREG)      ; Get address of string
	ex      (SP),HL         ; Save address of string
	push    HL              ; Save adjusted token value
	ex      DE,HL           ; Restore code string address
	call    GETINT          ; Get integer 0-255
	ex      DE,HL           ; Save code string address
	ex      (SP),HL         ; Save integer,HL = adj' token
	jr      GOFUNC          ; Jump to string function

FNVAL:  call    EVLPAR          ; Evaluate expression
	ex      (SP),HL         ; HL = Adjusted token value
	ld      DE,RETNUM       ; Return number from function
	push    DE              ; Save on stack
GOFUNC: ld      BC,FNCTAB       ; Function routine addresses
	add     HL,BC           ; Point to right address
	ld      C,(HL)          ; Get LSB of address
	inc     HL              ;
	ld      H,(HL)          ; Get MSB of address
	ld      L,C             ; Address to HL
	jp      (HL)            ; Jump to function

SGNEXP: dec     D               ; Dee to flag negative exponent
	cp      ZMINUS          ; '-' token ?
	ret     Z               ; Yes - Return
	cp      '-'             ; '-' ASCII ?
	ret     Z               ; Yes - Return
	inc     D               ; Inc to flag positive exponent
	cp      '+'             ; '+' ASCII ?
	ret     Z               ; Yes - Return
	cp      ZPLUS           ; '+' token ?
	ret     Z               ; Yes - Return
	dec     HL              ; dec 'cos GETCHR incs
	ret                     ; Return "NZ"

POR:    DB      0F6H            ; Flag "OR"
PAND:   xor      A               ; Flag "AND"
	push    AF              ; Save "AND" / "OR" flag
	call    TSTNUM          ; Make sure it's a number
	call    DEINT           ; Get integer -32768 to 32767
	pop     AF              ; Restore "AND" / "OR" flag
	ex      DE,HL           ; <- Get last
	pop     BC              ; <-  value
	ex      (SP),HL         ; <-  from
	ex      DE,HL           ; <-  stack
	call    FPBCDE          ; Move last value to FPREG
	push    AF              ; Save "AND" / "OR" flag
	call    DEINT           ; Get integer -32768 to 32767
	pop     AF              ; Restore "AND" / "OR" flag
	pop     BC              ; Get value
	ld      A,C             ; Get LSB
	ld      HL,ACPASS       ; Address of save AC as current
	jr      NZ,POR1         ; Jump if OR
	and     E               ; "AND" LSBs
	ld      C,A             ; Save LSB
	ld      A,B             ; Get MBS
	and     D               ; "AND" MSBs
	jp      (HL)            ; Save AC as current (ACPASS)

POR1:   or      E               ; "OR" LSBs
	ld      C,A             ; Save LSB
	ld      A,B             ; Get MSB
	or      D               ; "OR" MSBs
	jp      (HL)            ; Save AC as current (ACPASS)

TSTRED: ld      HL,CMPLOG       ; Logical compare routine
	ld      A,(TYPE)        ; Get data type
	rra                     ; Carry set = string
	ld      A,D             ; Get last precedence value
	rla                     ; Times 2 plus carry
	ld      E,A             ; To E
	ld      D,64H	; Relational precedence
	ld      A,B             ; Get current precedence
	cp      D               ; Compare with last
	ret     NC              ; Eval if last was rel' or log'
	jp      STKTHS          ; Stack this one and get next

CMPLOG: DW   CMPLG1          ; Compare two values / strings
CMPLG1: ld      A,C             ; Get data type
	or      A
	rra
	pop     BC              ; Get last expression to BCDE
	pop     DE
	push    AF              ; Save status
	call    CHKTYP          ; Check that types match
	ld      HL,CMPRES       ; Result to comparison
	push    HL              ; Save for return
	jp      Z,CMPNUM        ; Compare values if numeric
	xor      A               ; Compare two strings
	ld      (TYPE),A        ; Set type to numeric
	push    DE              ; Save string name
	call    GSTRCU          ; Get current string
	ld      A,(HL)          ; Get length of string
	inc     HL
	inc     HL
	ld      C,(HL)          ; Get LSB of address
	inc     HL
	ld      B,(HL)          ; Get MSB of address
	pop     DE              ; Restore string name
	push    BC              ; Save address of string
	push    AF              ; Save length of string
	call    GSTRDE          ; Get second string
	call    LOADFP          ; Get address of second string
	pop     AF              ; Restore length of string 1
	ld      D,A             ; Length to D
	pop     HL              ; Restore address of string 1
CMPSTR: ld      A,E             ; DBs of string 2 to do
	or      D               ; DBs of string 1 to do
	ret     Z               ; Exit if all DBs compared
	ld      A,D             ; Get DBs of string 1 to do
	sub     1
	ret     C               ; Exit if end of string 1
	xor      A
	cp      E               ; DBs of string 2 to do
	inc     A
	ret     NC              ; Exit if end of string 2
	dec     D               ; Count DBs in string 1
	dec     E               ; Count DBs in string 2
	ld      A,(BC)          ; DB in string 2
	cp      (HL)            ; Compare to DB in string 1
	inc     HL              ; Move up string 1
	inc     BC              ; Move up string 2
	jr      Z,CMPSTR        ; Same - Try next DBs
	CCF                     ; Flag difference (">" or "<")
	jp      FLGDIF          ; "<" gives -1 , ">" gives +1

CMPRES: inc     A               ; Increment current value
	adc     A,A             ; Double plus carry
	pop     BC              ; Get other value
	and     B               ; Combine them
	add     A,-1            ; Carry set if different
	sbc     A,A             ; 00 - Equal , FF - Different
	jp      FLGREL          ; Set current value & continue

EVNOT:  ld      D,5AH	; Precedence value for "NOT"
	call    EVAL1           ; Eval until precedence break
	call    TSTNUM          ; Make sure it's a number
	call    DEINT           ; Get integer -32768 - 32767
	ld      A,E             ; Get LSB
	cpl                     ; Invert LSB
	ld      C,A             ; Save "NOT" of LSB
	ld      A,D             ; Get MSB
	cpl                     ; Invert MSB
	call    ACPASS          ; Save AC as current
	pop     BC              ; Clean up stack
	jp      EVAL3           ; Continue evaluation

DIMRET: dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	ret     Z               ; End of DIM statement
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
DIM:    ld      BC,DIMRET       ; Return to "DIMRET"
	push    BC              ; Save on stack
	DB      0F6H            ; Flag "Create" variable
GETVAR: xor     A               ; Find variable address,to DE
	ld      (LCRFLG),A      ; Set locate / create flag
	ld      B,(HL)          ; Get First DB of name
GTFNAM: call    CHKLTR          ; See if a letter
	jp      C,SNERR         ; ?SN Error if not a letter
	xor      A
	ld      C,A             ; Clear second DB of name
	ld      (TYPE),A        ; Set type to numeric
	call    GETCHR          ; Get next character
	jr      C,SVNAM2        ; Numeric - Save in name
	call    CHKLTR          ; See if a letter
	jr      C,CHARTY        ; Not a letter - Check type
SVNAM2: ld      C,A             ; Save second DB of name
ENDNAM: call    GETCHR          ; Get next character
	jr      C,ENDNAM        ; Numeric - Get another
	call    CHKLTR          ; See if a letter
	jr      NC,ENDNAM       ; Letter - Get another
CHARTY: sub     '$'             ; String variable?
	jp      NZ,NOTSTR       ; No - Numeric variable
	inc     A               ; A = 1 (string type)
	ld      (TYPE),A        ; Set type to string
	rrca                    ; A = 80H , Flag for string
	add     A,C             ; 2nd DB of name has bit 7 on
	ld      C,A             ; Resave second DB on name
	call    GETCHR          ; Get next character
NOTSTR: ld      A,(FORFLG)      ; Array name needed ?
	dec     A
	jp      Z,ARLDSV        ; Yes - Get array name
	jp      P,NSCFOR        ; No array with "FOR" or "FN"
	ld      A,(HL)          ; Get DB again
	sub     '('             ; Subscripted variable?
	jp      Z,SBSCPT        ; Yes - Sort out subscript

NSCFOR: xor     A               ; Simple variable
	ld      (FORFLG),A      ; Clear "FOR" flag
	push    HL              ; Save code string address
	ld      D,B             ; DE = Variable name to find
	ld      E,C
	ld      HL,(FNRGNM)     ; FN argument name
	call    CPDEHL          ; Is it the FN argument?
	ld      DE,FNARG        ; Point to argument value
	jp      Z,POPHRT        ; Yes - Return FN argument value
	ld      HL,(VAREND)     ; End of variables
	ex      DE,HL           ; Address of end of search
	ld      HL,(PROGND)     ; Start of variables address
FNDVAR: call    CPDEHL          ; End of variable list table?
	jr      Z,CFEVAL        ; Yes - Called from EVAL?
	ld      A,C             ; Get second DB of name
	sub     (HL)            ; Compare with name in list
	inc     HL              ; Move on to first DB
	jr      NZ,FNTHR        ; Different - Find another
	ld      A,B             ; Get first DB of name
	sub     (HL)            ; Compare with name in list
FNTHR:  inc     HL              ; Move on to LSB of value
	jp      Z,RETADR        ; Found - Return address
	inc     HL              ; <- Skip
	inc     HL              ; <- over
	inc     HL              ; <- F.P.
	inc     HL              ; <- value
	jr      FNDVAR          ; Keep looking

CFEVAL: pop     HL              ; Restore code string address
	ex      (SP),HL         ; Get return address
	push    DE              ; Save address of variable
	ld      DE,FRMEVL       ; Return address in EVAL
	call    CPDEHL          ; Called from EVAL ?
	pop     DE              ; Restore address of variable
	jr      Z,RETNUL        ; Yes - Return null variable
	ex      (SP),HL         ; Put back return
	push    HL              ; Save code string address
	push    BC              ; Save variable name
	ld      BC,6            ; 2 DB name plus 4 DB data
	ld      HL,(ARREND)     ; End of arrays
	push    HL              ; Save end of arrays
	add     HL,BC           ; Move up 6 DBs
	pop     BC              ; Source address in BC
	push    HL              ; Save new end address
	call    MOVUP           ; Move arrays up
	pop     HL              ; Restore new end address
	ld      (ARREND),HL     ; Set new end address
	ld      H,B             ; End of variables to HL
	ld      L,C
	ld      (VAREND),HL     ; Set new end address

ZEROLP: dec     HL              ; Back through to zero variable
	ld      (HL),0          ; Zero DB in variable
	call    CPDEHL          ; Done them all?
	jr      NZ,ZEROLP       ; No - Keep on going
	pop     DE              ; Get variable name
	ld      (HL),E          ; Store second character
	inc     HL
	ld      (HL),D          ; Store first character
	inc     HL
RETADR: ex      DE,HL           ; Address of variable in DE
	pop     HL              ; Restore code string address
	ret

RETNUL: ld      (FPEXP),A       ; Set result to zero
	ld      HL,ZERBYT       ; Also set a null string
	ld      (FPREG),HL      ; Save for EVAL
	pop     HL              ; Restore code string address
	ret

SBSCPT: push    HL              ; Save code string address
	ld      HL,(LCRFLG)     ; Locate/Create and Type
	ex      (SP),HL         ; Save and get code string
	ld      D,A             ; Zero number of dimensions
SCPTLP: push    DE              ; Save number of dimensions
	push    BC              ; Save array name
	call    FPSINT          ; Get subscript (0-32767)
	pop     BC              ; Restore array name
	pop     AF              ; Get number of dimensions
	ex      DE,HL
	ex      (SP),HL         ; Save subscript value
	push    HL              ; Save LCRFLG and TYPE
	ex      DE,HL
	inc     A               ; Count dimensions
	ld      D,A             ; Save in D
	ld      A,(HL)          ; Get next DB in code string
	cp      ','             ; Comma (more to come)?
	jr      Z,SCPTLP        ; Yes - More subscripts
	call    CHKSYN          ; Make sure ")" follows
	DB      ")"
	ld      (NXTOPR),HL     ; Save code string address
	pop     HL              ; Get LCRFLG and TYPE
	ld      (LCRFLG),HL     ; Restore Locate/create & type
	ld      E,0             ; Flag not CSAVE* or CLOAD*
	push    DE              ; Save number of dimensions (D)
	DB      11H             ; Skip "push HL" and "push AF'

ARLDSV: push    HL              ; Save code string address
	push    AF              ; A = 00 , Flags set = Z,N
	ld      HL,(VAREND)     ; Start of arrays
	DB      3EH             ; Skip "add HL,DE"
FNDARY: add     HL,DE           ; Move to next array start
	ex      DE,HL
	ld      HL,(ARREND)     ; End of arrays
	ex      DE,HL           ; Current array pointer
	call    CPDEHL          ; End of arrays found?
	jr      Z,CREARY        ; Yes - Create array
	ld      A,(HL)          ; Get second DB of name
	cp      C               ; Compare with name given
	inc     HL              ; Move on
	jr      NZ,NXTARY       ; Different - Find next array
	ld      A,(HL)          ; Get first DB of name
	cp      B               ; Compare with name given
NXTARY: inc     HL              ; Move on
	ld      E,(HL)          ; Get LSB of next array address
	inc     HL
	ld      D,(HL)          ; Get MSB of next array address
	inc     HL
	jr      NZ,FNDARY       ; Not found - Keep looking
	ld      A,(LCRFLG)      ; Found Locate or Create it?
	or      A
	jp      NZ,DDERR        ; Create - ?DD Error
	pop     AF              ; Locate - Get number of dim'ns
	ld      B,H             ; BC Points to array dim'ns
	ld      C,L
	jp      Z,POPHRT        ; Jump if array load/save
	sub     (HL)            ; Same number of dimensions?
	jp      Z,FINDEL        ; Yes - Find element
BSERR:  ld      E,BS            ; ?BS Error
	jp      ERROR           ; Output error

CREARY: ld      DE,4            ; 4 DBs per entry
	pop     AF              ; Array to save or 0 dim'ns?
	jp      Z,FCERR         ; Yes - ?FC Error
	ld      (HL),C          ; Save second DB of name
	inc     HL
	ld      (HL),B          ; Save first DB of name
	inc     HL
	ld      C,A             ; Number of dimensions to C
	call    CHKSTK          ; Check if enough memory
	inc     HL              ; Point to number of dimensions
	inc     HL
	ld      (CUROPR),HL     ; Save address of pointer
	ld      (HL),C          ; Set number of dimensions
	inc     HL
	ld      A,(LCRFLG)      ; Locate of Create?
	rla                     ; Carry set = Create
	ld      A,C             ; Get number of dimensions
CRARLP: ld      BC,10+1         ; Default dimension size 10
	jr      NC,DEFSIZ       ; Locate - Set default size
	pop     BC              ; Get specified dimension size
	inc     BC              ; Include zero element
DEFSIZ: ld      (HL),C          ; Save LSB of dimension size
	inc     HL
	ld      (HL),B          ; Save MSB of dimension size
	inc     HL
	push    AF              ; Save num' of dim'ns an status
	push    HL              ; Save address of dim'n size
	call    MLDEBC          ; Multiply DE by BC to find
	ex      DE,HL           ; amount of mem needed (to DE)
	pop     HL              ; Restore address of dimension
	pop     AF              ; Restore number of dimensions
	dec     A               ; Count them
	jr      NZ,CRARLP       ; Do next dimension if more
	push    AF              ; Save locate/create flag
	ld      B,D             ; MSB of memory needed
	ld      C,E             ; LSB of memory needed
	ex      DE,HL
	add     HL,DE           ; Add DBs to array start
	jp      C,OMERR         ; Too big - Error
	call    ENFMEM          ; See if enough memory
	ld      (ARREND),HL     ; Save new end of array

ZERARY: dec     HL              ; Back through array data
	ld      (HL),0          ; Set array element to zero
	call    CPDEHL          ; All elements zeroed?
	jr      NZ,ZERARY       ; No - Keep on going
	inc     BC              ; Number of DBs + 1
	ld      D,A             ; A=0
	ld      HL,(CUROPR)     ; Get address of array
	ld      E,(HL)          ; Number of dimensions
	ex      DE,HL           ; To HL
	add     HL,HL           ; Two DBs per dimension size
	add     HL,BC           ; Add number of DBs
	ex      DE,HL           ; DBs needed to DE
	dec     HL
	dec     HL
	ld      (HL),E          ; Save LSB of DBs needed
	inc     HL
	ld      (HL),D          ; Save MSB of DBs needed
	inc     HL
	pop     AF              ; Locate / Create?
	jp      C,ENDDIM        ; A is 0 , End if create
FINDEL: ld      B,A             ; Find array element
	ld      C,A
	ld      A,(HL)          ; Number of dimensions
	inc     HL
	DB      16H             ; Skip "pop HL"
FNDELP: pop     HL              ; Address of next dim' size
	ld      E,(HL)          ; Get LSB of dim'n size
	inc     HL
	ld      D,(HL)          ; Get MSB of dim'n size
	inc     HL
	ex      (SP),HL         ; Save address - Get index
	push    AF              ; Save number of dim'ns
	call    CPDEHL          ; Dimension too large?
	jp      NC,BSERR        ; Yes - ?BS Error
	push    HL              ; Save index
	call    MLDEBC          ; Multiply previous by size
	pop     DE              ; Index supplied to DE
	add     HL,DE           ; Add index to pointer
	pop     AF              ; Number of dimensions
	dec     A               ; Count them
	ld      B,H             ; MSB of pointer
	ld      C,L             ; LSB of pointer
	jr      NZ,FNDELP       ; More - Keep going
	add     HL,HL           ; 4 DBs per element
	add     HL,HL
	pop     BC              ; Start of array
	add     HL,BC           ; Point to element
	ex      DE,HL           ; Address of element to DE
ENDDIM: ld      HL,(NXTOPR)     ; Got code string address
	ret

FRE:    ld      HL,(ARREND)     ; Start of free memory
	ex      DE,HL           ; To DE
	ld      HL,0            ; End of free memory
	add     HL,SP           ; Current stack value
	ld      A,(TYPE)        ; Dummy argument type
	or      A
	jp      Z,FRENUM        ; Numeric - Free variable space
	call    GSTRCU          ; Current string to pool
	call    GARBGE          ; Garbage collection
	ld      HL,(STRSPC)     ; Bottom of string space in use
	ex      DE,HL           ; To DE
	ld      HL,(STRBOT)     ; Bottom of string space
FRENUM: ld      A,L             ; Get LSB of end
	sub     E               ; Subtract LSB of beginning
	ld      C,A             ; Save difference if C
	ld      A,H             ; Get MSB of end
	sbc     A,D             ; Subtract MSB of beginning
ACPASS: ld      B,C             ; Return integer AC
ABPASS: ld      D,B             ; Return integer AB
	ld      E,0
	ld      HL,TYPE         ; Point to type
	ld      (HL),E          ; Set type to numeric
	ld      B,80H+16        ; 16 bit integer
	jp      RETINT          ; Return the integr

POS:    ld      A,(CURPOS)      ; Get cursor position
PASSA:  ld      B,A             ; Put A into AB
	xor      A               ; Zero A
	jr      ABPASS          ; Return integer AB

DEF:    call    CHEKFN          ; Get "FN" and name
	call    IDTEST          ; Test for illegal direct
	ld      BC,DATA         ; To get next statement
	push    BC              ; Save address for return
	push    DE              ; Save address of function ptr
	call    CHKSYN          ; Make sure "(" follows
	DB      "("
	call    GETVAR          ; Get argument variable name
	push    HL              ; Save code string address
	ex      DE,HL           ; Argument address to HL
	dec     HL
	ld      D,(HL)          ; Get first DB of arg name
	dec     HL
	ld      E,(HL)          ; Get second DB of arg name
	pop     HL              ; Restore code string address
	call    TSTNUM          ; Make sure numeric argument
	call    CHKSYN          ; Make sure ")" follows
	DB      ")"
	call    CHKSYN          ; Make sure "=" follows
	DB      ZEQUAL          ; "=" token
	ld      B,H             ; Code string address to BC
	ld      C,L
	ex      (SP),HL         ; Save code str , Get FN ptr
	ld      (HL),C          ; Save LSB of FN code string
	inc     HL
	ld      (HL),B          ; Save MSB of FN code string
	jp      SVSTAD          ; Save address and do function

DOFN:   call    CHEKFN          ; Make sure FN follows
	push    DE              ; Save function pointer address
	call    EVLPAR          ; Evaluate expression in "()"
	call    TSTNUM          ; Make sure numeric result
	ex      (SP),HL         ; Save code str , Get FN ptr
	ld      E,(HL)          ; Get LSB of FN code string
	inc     HL
	ld      D,(HL)          ; Get MSB of FN code string
	inc     HL
	ld      A,D             ; And function DEFined?
	or      E
	jp      Z,UFERR         ; No - ?UF Error
	ld      A,(HL)          ; Get LSB of argument address
	inc     HL
	ld      H,(HL)          ; Get MSB of argument address
	ld      L,A             ; HL = Arg variable address
	push    HL              ; Save it
	ld      HL,(FNRGNM)     ; Get old argument name
	ex      (SP),HL ;       ; Save old , Get new
	ld      (FNRGNM),HL     ; Set new argument name
	ld      HL,(FNARG+2)    ; Get LSB,NLSB of old arg value
	push    HL              ; Save it
	ld      HL,(FNARG)      ; Get MSB,EXP of old arg value
	push    HL              ; Save it
	ld      HL,FNARG        ; HL = Value of argument
	push    DE              ; Save FN code string address
	call    FPTHL           ; Move FPREG to argument
	pop     HL              ; Get FN code string address
	call    GETNUM          ; Get value from function
	dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	jp      NZ,SNERR        ; Bad character in FN - Error
	pop     HL              ; Get MSB,EXP of old arg
	ld      (FNARG),HL      ; Restore it
	pop     HL              ; Get LSB,NLSB of old arg
	ld      (FNARG+2),HL    ; Restore it
	pop     HL              ; Get name of old arg
	ld      (FNRGNM),HL     ; Restore it
	pop     HL              ; Restore code string address
	ret

IDTEST: push    HL              ; Save code string address
	ld      HL,(LINEAT)     ; Get current line number
	inc     HL              ; -1 means direct statement
	ld      A,H
	or      L
	pop     HL              ; Restore code string address
	ret     NZ              ; Return if in program
	ld      E,ID            ; ?ID Error
	jp      ERROR

CHEKFN: call    CHKSYN          ; Make sure FN follows
	DB      ZFN             ; "FN" token
	ld      A,80H
	ld      (FORFLG),A      ; Flag FN name to find
	or      (HL)            ; FN name has bit 7 set
	ld      B,A             ; in first DB of name
	call    GTFNAM          ; Get FN name
	jp      TSTNUM          ; Make sure numeric function

STR:    call    TSTNUM          ; Make sure it's a number
	call    NUMASC          ; Turn number into text
STR1:   call    CRTST           ; Create string entry for it
	call    GSTRCU          ; Current string to pool
	ld      BC,TOPOOL       ; Save in string pool
	push    BC              ; Save address on stack

SAVSTR: ld      A,(HL)          ; Get string length
	inc     HL
	inc     HL
	push    HL              ; Save pointer to string
	call    TESTR           ; See if enough string space
	pop     HL              ; Restore pointer to string
	ld      C,(HL)          ; Get LSB of address
	inc     HL
	ld      B,(HL)          ; Get MSB of address
	call    CRTMST          ; Create string entry
	push    HL              ; Save pointer to MSB of addr
	ld      L,A             ; Length of string
	call    TOSTRA          ; Move to string area
	pop     DE              ; Restore pointer to MSB
	ret

MKTMST: call    TESTR           ; See if enough string space
CRTMST: ld      HL,TMPSTR       ; Temporary string
	push    HL              ; Save it
	ld      (HL),A          ; Save length of string
	inc     HL
SVSTAD: inc     HL
	ld      (HL),E          ; Save LSB of address
	inc     HL
	ld      (HL),D          ; Save MSB of address
	pop     HL              ; Restore pointer
	ret

CRTST:  dec     HL              ; dec - inced after
QTSTR:  ld      B,'"'           ; Terminating quote
	ld      D,B             ; Quote to D
DTSTR:  push    HL              ; Save start
	ld      C,-1            ; Set counter to -1
QTSTLP: inc     HL              ; Move on
	ld      A,(HL)          ; Get DB
	inc     C               ; Count DBs
	or      A               ; End of line?
	jr      Z,CRTSTE        ; Yes - Create string entry
	cp      D               ; Terminator D found?
	jr      Z,CRTSTE        ; Yes - Create string entry
	cp      B               ; Terminator B found?
	jr      NZ,QTSTLP       ; No - Keep looking
CRTSTE: cp      '"'             ; End with '"'?
	call    Z,GETCHR        ; Yes - Get next character
	ex      (SP),HL         ; Starting quote
	inc     HL              ; First DB of string
	ex      DE,HL           ; To DE
	ld      A,C             ; Get length
	call    CRTMST          ; Create string entry
TSTOPL: ld      DE,TMPSTR       ; Temporary string
	ld      HL,(TMSTPT)     ; Temporary string pool pointer
	ld      (FPREG),HL      ; Save address of string ptr
	ld      A,1
	ld      (TYPE),A        ; Set type to string
	call    DETHL4          ; Move string to pool
	call    CPDEHL          ; Out of string pool?
	ld      (TMSTPT),HL     ; Save new pointer
	pop     HL              ; Restore code string address
	ld      A,(HL)          ; Get next code DB
	ret     NZ              ; Return if pool OK
	ld      E,ST            ; ?ST Error
	jp      ERROR           ; String pool overflow

PRNUMS: inc     HL              ; Skip leading space
PRS:    call    CRTST           ; Create string entry for it
PRS1:   call    GSTRCU          ; Current string to pool
	call    LOADFP          ; Move string block to BCDE
	inc     E               ; Length + 1
PRSLP:  dec     E               ; Count characters
	ret     Z               ; End of string
	ld      A,(BC)          ; Get DB to output
	call    OUTC            ; Output character in A
	cp      CR              ; Return?
	call    Z,DONULL        ; Yes - Do nulls
	inc     BC              ; Next DB in string
	jp      PRSLP           ; More characters to output

TESTR:  or      A               ; Test if enough room
	DB      0EH             ; No garbage collection done
GRBDON: pop     AF              ; Garbage collection done
	push    AF              ; Save status
	ld      HL,(STRSPC)     ; Bottom of string space in use
	ex      DE,HL           ; To DE
	ld      HL,(STRBOT)     ; Bottom of string area
	cpl                     ; Negate length (Top down)
	ld      C,A             ; -Length to BC
	ld      B,-1            ; BC = -ve length of string
	add     HL,BC           ; Add to bottom of space in use
	inc     HL              ; Plus one for 2's complement
	call    CPDEHL          ; Below string RAM area?
	jr      C,TESTOS        ; Tidy up if not done else err
	ld      (STRBOT),HL     ; Save new bottom of area
	inc     HL              ; Point to first DB of string
	ex      DE,HL           ; Address to DE
POPAF:  pop     AF              ; Throw away status push
	ret

TESTOS: pop     AF              ; Garbage collect been done?
	ld      E,OS            ; ?OS Error
	jp      Z,ERROR         ; Yes - Not enough string apace
	cp      A               ; Flag garbage collect done
	push    AF              ; Save status
	ld      BC,GRBDON       ; Garbage collection done
	push    BC              ; Save for return
GARBGE: ld      HL,(LSTRAM)     ; Get end of RAM pointer
GARBLP: ld      (STRBOT),HL     ; Reset string pointer
	ld      HL,0
	push    HL              ; Flag no string found
	ld      HL,(STRSPC)     ; Get bottom of string space
	push    HL              ; Save bottom of string space
	ld      HL,TMSTPL       ; Temporary string pool
GRBLP:  ex      DE,HL
	ld      HL,(TMSTPT)     ; Temporary string pool pointer
	ex      DE,HL
	call    CPDEHL          ; Temporary string pool done?
	ld      BC,GRBLP        ; Loop until string pool done
	jp      NZ,STPOOL       ; No - See if in string area
	ld      HL,(PROGND)     ; Start of simple variables
SMPVAR: ex      DE,HL
	ld      HL,(VAREND)     ; End of simple variables
	ex      DE,HL
	call    CPDEHL          ; All simple strings done?
	jr      Z,ARRLP         ; Yes - Do string arrays
	ld      A,(HL)          ; Get type of variable
	inc     HL
	inc     HL
	or      A               ; "S" flag set if string
	call    STRADD          ; See if string in string area
	jr      SMPVAR          ; Loop until simple ones done

GNXARY: pop     BC              ; Scrap address of this array
ARRLP:  ex      DE,HL
	ld      HL,(ARREND)     ; End of string arrays
	ex      DE,HL
	call    CPDEHL          ; All string arrays done?
	jp      Z,SCNEND        ; Yes - Move string if found
	call    LOADFP          ; Get array name to BCDE
	ld      A,E             ; Get type of array     
	push    HL              ; Save address of num of dim'ns
	add     HL,BC           ; Start of next array
	or      A               ; Test type of array
	jp      P,GNXARY        ; Numeric array - Ignore it
	ld      (CUROPR),HL     ; Save address of next array
	pop     HL              ; Get address of num of dim'ns
	ld      C,(HL)          ; BC = Number of dimensions
	ld      B,0
	add     HL,BC           ; Two DBs per dimension size
	add     HL,BC
	inc     HL              ; Plus one for number of dim'ns
GRBARY: ex      DE,HL
	ld      HL,(CUROPR)     ; Get address of next array
	ex      DE,HL
	call    CPDEHL          ; Is this array finished?
	jr      Z,ARRLP         ; Yes - Get next one
	ld      BC,GRBARY       ; Loop until array all done
STPOOL: push    BC              ; Save return address
	or      80H             ; Flag string type
STRADD: ld      A,(HL)          ; Get string length
	inc     HL
	inc     HL
	ld      E,(HL)          ; Get LSB of string address
	inc     HL
	ld      D,(HL)          ; Get MSB of string address
	inc     HL
	ret     P               ; Not a string - Return
	or      A               ; Set flags on string length
	ret     Z               ; Null string - Return
	ld      B,H             ; Save variable pointer
	ld      C,L
	ld      HL,(STRBOT)     ; Bottom of new area
	call    CPDEHL          ; String been done?
	ld      H,B             ; Restore variable pointer
	ld      L,C
	ret     C               ; String done - Ignore
	pop     HL              ; Return address
	ex      (SP),HL         ; Lowest available string area
	call    CPDEHL          ; String within string area?
	ex      (SP),HL         ; Lowest available string area
	push    HL              ; Re-save return address
	ld      H,B             ; Restore variable pointer
	ld      L,C
	ret     NC              ; Outside string area - Ignore
	pop     BC              ; Get return , Throw 2 away
	pop     AF              ; 
	pop     AF              ; 
	push    HL              ; Save variable pointer
	push    DE              ; Save address of current
	push    BC              ; Put back return address
	ret                     ; Go to it

SCNEND: pop     DE              ; Addresses of strings
	pop     HL              ; 
	ld      A,L             ; HL = 0 if no more to do
	or      H
	ret     Z               ; No more to do - Return
	dec     HL
	ld      B,(HL)          ; MSB of address of string
	dec     HL
	ld      C,(HL)          ; LSB of address of string
	push    HL              ; Save variable address
	dec     HL
	dec     HL
	ld      L,(HL)          ; HL = Length of string
	ld      H,0
	add     HL,BC           ; Address of end of string+1
	ld      D,B             ; String address to DE
	ld      E,C
	dec     HL              ; Last DB in string
	ld      B,H             ; Address to BC
	ld      C,L
	ld      HL,(STRBOT)     ; Current bottom of string area
	call    MOVSTR          ; Move string to new address
	pop     HL              ; Restore variable address
	ld      (HL),C          ; Save new LSB of address
	inc     HL
	ld      (HL),B          ; Save new MSB of address
	ld      L,C             ; Next string area+1 to HL
	ld      H,B
	dec     HL              ; Next string area address
	jp      GARBLP          ; Look for more strings

CONCAT: push    BC              ; Save prec' opr & code string
	push    HL              ; 
	ld      HL,(FPREG)      ; Get first string
	ex      (SP),HL         ; Save first string
	call    OPRND           ; Get second string
	ex      (SP),HL         ; Restore first string
	call    TSTSTR          ; Make sure it's a string
	ld      A,(HL)          ; Get length of second string
	push    HL              ; Save first string
	ld      HL,(FPREG)      ; Get second string
	push    HL              ; Save second string
	add     A,(HL)          ; Add length of second string
	ld      E,LS            ; ?LS Error
	jp      C,ERROR         ; String too long - Error
	call    MKTMST          ; Make temporary string
	pop     DE              ; Get second string to DE
	call    GSTRDE          ; Move to string pool if needed
	ex      (SP),HL         ; Get first string
	call    GSTRHL          ; Move to string pool if needed
	push    HL              ; Save first string
	ld      HL,(TMPSTR+2)   ; Temporary string address
	ex      DE,HL           ; To DE
	call    SSTSA           ; First string to string area
	call    SSTSA           ; Second string to string area
	ld      HL,EVAL2        ; Return to evaluation loop
	ex      (SP),HL         ; Save return,get code string
	push    HL              ; Save code string address
	jp      TSTOPL          ; To temporary string to pool

SSTSA:  pop     HL              ; Return address
	ex      (SP),HL         ; Get string block,save return
	ld      A,(HL)          ; Get length of string
	inc     HL
	inc     HL
	ld      C,(HL)          ; Get LSB of string address
	inc     HL
	ld      B,(HL)          ; Get MSB of string address
	ld      L,A             ; Length to L
TOSTRA: inc     L               ; inc - deced after
TSALP:  dec     L               ; Count DBs moved
	ret     Z               ; End of string - Return
	ld      A,(BC)          ; Get source
	ld      (DE),A          ; Save destination
	inc     BC              ; Next source
	inc     DE              ; Next destination
	jr      TSALP           ; Loop until string moved

GETSTR: call    TSTSTR          ; Make sure it's a string
GSTRCU: ld      HL,(FPREG)      ; Get current string
GSTRHL: ex      DE,HL           ; Save DE
GSTRDE: call    BAKTMP          ; Was it last tmp-str?
	ex      DE,HL           ; Restore DE
	ret     NZ              ; No - Return
	push    DE              ; Save string
	ld      D,B             ; String block address to DE
	ld      E,C
	dec     DE              ; Point to length
	ld      C,(HL)          ; Get string length
	ld      HL,(STRBOT)     ; Current bottom of string area
	call    CPDEHL          ; Last one in string area?
	jr      NZ,POPHL        ; No - Return
	ld      B,A             ; Clear B (A=0)
	add     HL,BC           ; Remove string from str' area
	ld      (STRBOT),HL     ; Save new bottom of str' area
POPHL:  pop     HL              ; Restore string
	ret

BAKTMP: ld      HL,(TMSTPT)     ; Get temporary string pool top
	dec     HL              ; Back
	ld      B,(HL)          ; Get MSB of address
	dec     HL              ; Back
	ld      C,(HL)          ; Get LSB of address
	dec     HL              ; Back
	dec     HL              ; Back
	call    CPDEHL          ; String last in string pool?
	ret     NZ              ; Yes - Leave it
	ld      (TMSTPT),HL     ; Save new string pool top
	ret

LEN:    ld      BC,PASSA        ; To return integer A
	push    BC              ; Save address
GETLEN: call    GETSTR          ; Get string and its length
	xor      A
	ld      D,A             ; Clear D
	ld      (TYPE),A        ; Set type to numeric
	ld      A,(HL)          ; Get length of string
	or      A               ; Set status flags
	ret

ASC:    ld      BC,PASSA        ; To return integer A
	push    BC              ; Save address
GTFLNM: call    GETLEN          ; Get length of string
	jp      Z,FCERR         ; Null string - Error
	inc     HL
	inc     HL
	ld      E,(HL)          ; Get LSB of address
	inc     HL
	ld      D,(HL)          ; Get MSB of address
	ld      A,(DE)          ; Get first DB of string
	ret

CHR:    ld      A,1             ; One character string
	call    MKTMST          ; Make a temporary string
	call    MAKINT          ; Make it integer A
	ld      HL,(TMPSTR+2)   ; Get address of string
	ld      (HL),E          ; Save character
TOPOOL: pop     BC              ; Clean up stack
	jp      TSTOPL          ; Temporary string to pool

LEFT:   call    LFRGNM          ; Get number and ending ")"
	xor      A               ; Start at first DB in string
RIGHT1: ex      (SP),HL         ; Save code string,Get string
	ld      C,A             ; Starting position in string
MID1:   push    HL              ; Save string block address
	ld      A,(HL)          ; Get length of string
	cp      B               ; Compare with number given
	jr      C,ALLFOL        ; All following DBs required
	ld      A,B             ; Get new length
	DB      11H             ; Skip "ld C,0"
ALLFOL: ld      C,0             ; First DB of string
	push    BC              ; Save position in string
	call    TESTR           ; See if enough string space
	pop     BC              ; Get position in string
	pop     HL              ; Restore string block address
	push    HL              ; And re-save it
	inc     HL
	inc     HL
	ld      B,(HL)          ; Get LSB of address
	inc     HL
	ld      H,(HL)          ; Get MSB of address
	ld      L,B             ; HL = address of string
	ld      B,0             ; BC = starting address
	add     HL,BC           ; Point to that DB
	ld      B,H             ; BC = source string
	ld      C,L
	call    CRTMST          ; Create a string entry
	ld      L,A             ; Length of new string
	call    TOSTRA          ; Move string to string area
	pop     DE              ; Clear stack
	call    GSTRDE          ; Move to string pool if needed
	jp      TSTOPL          ; Temporary string to pool

RIGHT:  call    LFRGNM          ; Get number and ending ")"
	pop     DE              ; Get string length
	push    DE              ; And re-save
	ld      A,(DE)          ; Get length
	sub     B               ; Move back N DBs
	jp      RIGHT1          ; Go and get sub-string

MID:    ex      DE,HL           ; Get code string address
	ld      A,(HL)          ; Get next DB ',' or ")"
	call    MIDNUM          ; Get number supplied
	inc     B               ; Is it character zero?
	dec     B
	jp      Z,FCERR         ; Yes - Error
	push    BC              ; Save starting position
	ld      E,255           ; All of string
	cp      ')'             ; Any length given?
	jp      Z,RSTSTR        ; No - Rest of string
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	call    GETINT          ; Get integer 0-255
RSTSTR: call    CHKSYN          ; Make sure ")" follows
	DB      ")"
	pop     AF              ; Restore starting position
	ex      (SP),HL         ; Get string,8ave code string
	ld      BC,MID1         ; Continuation of MID$ routine
	push    BC              ; Save for return
	dec     A               ; Starting position-1
	cp      (HL)            ; Compare with length
	ld      B,0             ; Zero DBs length
	ret     NC              ; Null string if start past end
	ld      C,A             ; Save starting position-1
	ld      A,(HL)          ; Get length of string
	sub     C               ; Subtract start
	cp      E               ; Enough string for it?
	ld      B,A             ; Save maximum length available
	ret     C               ; Truncate string if needed
	ld      B,E             ; Set specified length
	ret                     ; Go and create string

VAL:    call    GETLEN          ; Get length of string
	jp      Z,RESZER        ; Result zero
	ld      E,A             ; Save length
	inc     HL
	inc     HL
	ld      A,(HL)          ; Get LSB of address
	inc     HL
	ld      H,(HL)          ; Get MSB of address
	ld      L,A             ; HL = String address
	push    HL              ; Save string address
	add     HL,DE
	ld      B,(HL)          ; Get end of string+1 DB
	ld      (HL),D          ; Zero it to terminate
	ex      (SP),HL         ; Save string end,get start
	push    BC              ; Save end+1 DB
	ld      A,(HL)          ; Get starting DB
	cp	'$'		; HEX number indicated? [function added]
	jp	NZ,VAL1
	call	HEXTFP		; Convert HEX to FPREG
	jr	VAL3
VAL1:	cp	'%'		; Binary number indicated? [function added]
	jp	NZ,VAL2
	call	BINTFP		; Convert Bin to FPREG
	jr	VAL3
VAL2:   call	ASCTFP          ; Convert ASCII string to FP
VAL3:   pop     BC              ; Restore end+1 DB
	pop     HL              ; Restore end+1 address
	ld      (HL),B          ; Put back original DB
	ret

LFRGNM: ex      DE,HL           ; Code string address to HL
	call    CHKSYN          ; Make sure ")" follows
	DB      ")"
MIDNUM: pop     BC              ; Get return address
	pop     DE              ; Get number supplied
	push    BC              ; Re-save return address
	ld      B,E             ; Number to B
	ret

INP:    call    MAKINT          ; Make it integer A
	ld      (INPORT),A      ; Set input port
	call    INPSUB          ; Get input from port
	jp      PASSA           ; Return integer A

POUT:   call    SETIO           ; Set up port number
	jp      OUTSUB          ; Output data and return

WAIT:   call    SETIO           ; Set up port number
	push    AF              ; Save and mask
	ld      E,0             ; Assume zero if none given
	dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	jr      Z,NOXOR         ; No XOR DB given
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	call    GETINT          ; Get integer 0-255 to XOR with
NOXOR:  pop     BC              ; Restore and mask
WAITLP: call    INPSUB          ; Get input
	xor      E               ; Flip selected bits
	and     B               ; Result non-zero?
	jr      Z,WAITLP        ; No = keep waiting
	ret

SETIO:  call    GETINT          ; Get integer 0-255
	ld      (INPORT),A      ; Set input port
	ld      (OTPORT),A      ; Set output port
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	jr      GETINT          ; Get integer 0-255 and return

FNDNUM: call    GETCHR          ; Get next character
GETINT: call    GETNUM          ; Get a number from 0 to 255
MAKINT: call    DEPINT          ; Make sure value 0 - 255
	ld      A,D             ; Get MSB of number
	or      A               ; Zero?
	jp      NZ,FCERR        ; No - Error
	dec     HL              ; dec 'cos GETCHR incs
	call    GETCHR          ; Get next character
	ld      A,E             ; Get number to A
	ret

PEEK:   call    DEINT           ; Get memory address
	ld      A,(DE)          ; Get DB in memory
	jp      PASSA           ; Return integer A

POKE:   call    GETNUM          ; Get memory address
	call    DEINT           ; Get integer -32768 to 3276
	push    DE              ; Save memory address
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	call    GETINT          ; Get integer 0-255
	pop     DE              ; Restore memory address
	ld      (DE),A          ; Load it into memory
	ret

ROUND:  ld      HL,HALF         ; Add 0.5 to FPREG
ADDPHL: call    LOADFP          ; Load FP at (HL) to BCDE
	jr      FPADD           ; Add BCDE to FPREG

SUBPHL: call    LOADFP          ; FPREG = -FPREG + number at HL
	DB      21H             ; Skip "pop BC" and "pop DE"
PSUB:   pop     BC              ; Get FP number from stack
	pop     DE
SUBCDE: call    INVSGN          ; Negate FPREG
FPADD:  ld      A,B             ; Get FP exponent
	or      A               ; Is number zero?
	ret     Z               ; Yes - Nothing to add
	ld      A,(FPEXP)       ; Get FPREG exponent
	or      A               ; Is this number zero?
	jp      Z,FPBCDE        ; Yes - Move BCDE to FPREG
	sub     B               ; BCDE number larger?
	jr      NC,NOSWAP       ; No - Don't swap them
	cpl                     ; Two's complement
	inc     A               ;  FP exponent
	ex      DE,HL
	call    STAKFP          ; Put FPREG on stack
	ex      DE,HL
	call    FPBCDE          ; Move BCDE to FPREG
	pop     BC              ; Restore number from stack
	pop     DE
NOSWAP: cp      24+1            ; Second number insignificant?
	ret     NC              ; Yes - First number is result
	push    AF              ; Save number of bits to scale
	call    SIGNS           ; Set MSBs & sign of result
	ld      H,A             ; Save sign of result
	pop     AF              ; Restore scaling factor
	call    SCALE           ; Scale BCDE to same exponent
	or      H               ; Result to be positive?
	ld      HL,FPREG        ; Point to FPREG
	jp      P,MINCDE        ; No - Subtract FPREG from CDE
	call    PLUCDE          ; Add FPREG to CDE
	jp      NC,RONDUP       ; No overflow - Round it up
	inc     HL              ; Point to exponent
	inc     (HL)            ; Increment it
	jp      Z,OVERR         ; Number overflowed - Error
	ld      L,1             ; 1 bit to shift right
	call    SHRT1           ; Shift result right
	jp      RONDUP          ; Round it up

MINCDE: xor     A               ; Clear A and carry
	sub     B               ; Negate exponent
	ld      B,A             ; Re-save exponent
	ld      A,(HL)          ; Get LSB of FPREG
	sbc     A, E            ; Subtract LSB of BCDE
	ld      E,A             ; Save LSB of BCDE
	inc     HL
	ld      A,(HL)          ; Get NMSB of FPREG
	sbc     A,D             ; Subtract NMSB of BCDE
	ld      D,A             ; Save NMSB of BCDE
	inc     HL
	ld      A,(HL)          ; Get MSB of FPREG
	sbc     A,C             ; Subtract MSB of BCDE
	ld      C,A             ; Save MSB of BCDE
CONPOS: call    C,COMPL         ; Overflow - Make it positive

BNORM:  ld      L,B             ; L = Exponent
	ld      H,E             ; H = LSB
	xor      A
BNRMLP: ld      B,A             ; Save bit count
	ld      A,C             ; Get MSB
	or      A               ; Is it zero?
	jp      NZ,PNORM        ; No - Do it bit at a time
	ld      C,D             ; MSB = NMSB
	ld      D,H             ; NMSB= LSB
	ld      H,L             ; LSB = VLSB
	ld      L,A             ; VLSB= 0
	ld      A,B             ; Get exponent
	sub     8               ; Count 8 bits
	cp      -24-8           ; Was number zero?
	jr      NZ,BNRMLP       ; No - Keep normalising
RESZER: xor     A               ; Result is zero
SAVEXP: ld      (FPEXP),A       ; Save result as zero
	ret

NORMAL: dec     B               ; Count bits
	add     HL,HL           ; Shift HL left
	ld      A,D             ; Get NMSB
	rla                     ; Shift left with last bit
	ld      D,A             ; Save NMSB
	ld      A,C             ; Get MSB
	adc     A,A             ; Shift left with last bit
	ld      C,A             ; Save MSB
PNORM:  jp      P,NORMAL        ; Not done - Keep going
	ld      A,B             ; Number of bits shifted
	ld      E,H             ; Save HL in EB
	ld      B,L
	or      A               ; Any shifting done?
	jr      Z,RONDUP        ; No - Round it up
	ld      HL,FPEXP        ; Point to exponent
	add     A,(HL)          ; Add shifted bits
	ld      (HL),A          ; Re-save exponent
	jp      NC,RESZER       ; Underflow - Result is zero
	ret     Z               ; Result is zero
RONDUP: ld      A,B             ; Get VLSB of number
RONDB:  ld      HL,FPEXP        ; Point to exponent
	or      A               ; Any rounding?
	call    M,FPROND        ; Yes - Round number up
	ld      B,(HL)          ; B = Exponent
	inc     HL
	ld      A,(HL)          ; Get sign of result
	and     10000000B       ; Only bit 7 needed
	xor      C               ; Set correct sign
	ld      C,A             ; Save correct sign in number
	jp      FPBCDE          ; Move BCDE to FPREG

FPROND: inc     E               ; Round LSB
	ret     NZ              ; Return if ok
	inc     D               ; Round NMSB
	ret     NZ              ; Return if ok
	inc     C               ; Round MSB
	ret     NZ              ; Return if ok
	ld      C,80H	; Set normal value
	inc     (HL)            ; Increment exponent
	ret     NZ              ; Return if ok
	jp      OVERR           ; Overflow error

PLUCDE: ld      A,(HL)          ; Get LSB of FPREG
	add     A,E             ; Add LSB of BCDE
	ld      E,A             ; Save LSB of BCDE
	inc     HL
	ld      A,(HL)          ; Get NMSB of FPREG
	adc     A,D             ; Add NMSB of BCDE
	ld      D,A             ; Save NMSB of BCDE
	inc     HL
	ld      A,(HL)          ; Get MSB of FPREG
	adc     A,C             ; Add MSB of BCDE
	ld      C,A             ; Save MSB of BCDE
	ret

COMPL:  ld      HL,SGNRES       ; Sign of result
	ld      A,(HL)          ; Get sign of result
	cpl                     ; Negate it
	ld      (HL),A          ; Put it back
	xor      A
	ld      L,A             ; Set L to zero
	sub     B               ; Negate exponent,set carry
	ld      B,A             ; Re-save exponent
	ld      A,L             ; Load zero
	sbc     A,E             ; Negate LSB
	ld      E,A             ; Re-save LSB
	ld      A,L             ; Load zero
	sbc     A,D             ; Negate NMSB
	ld      D,A             ; Re-save NMSB
	ld      A,L             ; Load zero
	sbc     A,C             ; Negate MSB
	ld      C,A             ; Re-save MSB
	ret

SCALE:  ld      B,0             ; Clear underflow
SCALLP: sub     8               ; 8 bits (a whole DB)?
	jr      C,SHRITE        ; No - Shift right A bits
	ld      B,E             ; <- Shift
	ld      E,D             ; <- right
	ld      D,C             ; <- eight
	ld      C,0             ; <- bits
	jr      SCALLP          ; More bits to shift

SHRITE: add     A,8+1           ; Adjust count
	ld      L,A             ; Save bits to shift
SHRLP:  xor     A               ; Flag for all done
	dec     L               ; All shifting done?
	ret     Z               ; Yes - Return
	ld      A,C             ; Get MSB
SHRT1:  rra                     ; Shift it right
	ld      C,A             ; Re-save
	ld      A,D             ; Get NMSB
	rra                     ; Shift right with last bit
	ld      D,A             ; Re-save it
	ld      A,E             ; Get LSB
	rra                     ; Shift right with last bit
	ld      E,A             ; Re-save it
	ld      A,B             ; Get underflow
	rra                     ; Shift right with last bit
	ld      B,A             ; Re-save underflow
	jr      SHRLP           ; More bits to do

UNITY:  DB       000H,000H,000H,081H    ; 1.00000

LOGTAB: DB      3                       ; Table used by LOG
	DB      0AAH,056H,019H,080H     ; 0.59898
	DB      0F1H,022H,076H,080H     ; 0.96147
	DB      045H,0AAH,038H,082H     ; 2.88539

LOG:    call    TSTSGN          ; Test sign of value
	or      A
	jp      PE,FCERR        ; ?FC Error if <= zero
	ld      HL,FPEXP        ; Point to exponent
	ld      A,(HL)          ; Get exponent
	ld      BC,8035H        ; BCDE = SQR(1/2)
	ld      DE,04F3H
	sub     B               ; Scale value to be < 1
	push    AF              ; Save scale factor
	ld      (HL),B          ; Save new exponent
	push    DE              ; Save SQR(1/2)
	push    BC
	call    FPADD           ; Add SQR(1/2) to value
	pop     BC              ; Restore SQR(1/2)
	pop     DE
	inc     B               ; Make it SQR(2)
	call    DVBCDE          ; Divide by SQR(2)
	ld      HL,UNITY        ; Point to 1.
	call    SUBPHL          ; Subtract FPREG from 1
	ld      HL,LOGTAB       ; Coefficient table
	call    SUMSER          ; Evaluate sum of series
	ld      BC,8080H        ; BCDE = -0.5
	ld      DE,0000H
	call    FPADD           ; Subtract 0.5 from FPREG
	pop     AF              ; Restore scale factor
	call    RSCALE          ; Re-scale number
MULLN2: ld      BC,8031H        ; BCDE = Ln(2)
	ld      DE,7218H
	DB      21H             ; Skip "pop BC" and "pop DE"

MULT:   pop     BC              ; Get number from stack
	pop     DE
FPMULT: call    TSTSGN          ; Test sign of FPREG
	ret     Z               ; Return zero if zero
	ld      L,0             ; Flag add exponents
	call    ADDEXP          ; Add exponents
	ld      A,C             ; Get MSB of multiplier
	ld      (MULVAL),A      ; Save MSB of multiplier
	ex      DE,HL
	ld      (MULVAL+1),HL   ; Save rest of multiplier
	ld      BC,0            ; Partial product (BCDE) = zero
	ld      D,B
	ld      E,B
	ld      HL,BNORM        ; Address of normalise
	push    HL              ; Save for return
	ld      HL,MULT8        ; Address of 8 bit multiply
	push    HL              ; Save for NMSB,MSB
	push    HL              ; 
	ld      HL,FPREG        ; Point to number
MULT8:  ld      A,(HL)          ; Get LSB of number
	inc     HL              ; Point to NMSB
	or      A               ; Test LSB
	jp      Z,BYTSFT        ; Zero - shift to next DB
	push    HL              ; Save address of number
	ld      L,8             ; 8 bits to multiply by
MUL8LP: rra                     ; Shift LSB right
	ld      H,A             ; Save LSB
	ld      A,C             ; Get MSB
	jp      NC,NOMADD       ; Bit was zero - Don't add
	push    HL              ; Save LSB and count
	ld      HL,(MULVAL+1)   ; Get LSB and NMSB
	add     HL,DE           ; Add NMSB and LSB
	ex      DE,HL           ; Leave sum in DE
	pop     HL              ; Restore MSB and count
	ld      A,(MULVAL)      ; Get MSB of multiplier
	adc     A,C             ; Add MSB
NOMADD: rra                     ; Shift MSB right
	ld      C,A             ; Re-save MSB
	ld      A,D             ; Get NMSB
	rra                     ; Shift NMSB right
	ld      D,A             ; Re-save NMSB
	ld      A,E             ; Get LSB
	rra                     ; Shift LSB right
	ld      E,A             ; Re-save LSB
	ld      A,B             ; Get VLSB
	rra                     ; Shift VLSB right
	ld      B,A             ; Re-save VLSB
	dec     L               ; Count bits multiplied
	ld      A,H             ; Get LSB of multiplier
	jp      NZ,MUL8LP       ; More - Do it
POPHRT: pop     HL              ; Restore address of number
	ret

BYTSFT: ld      B,E             ; Shift partial product left
	ld      E,D
	ld      D,C
	ld      C,A
	ret

DIV10:  call    STAKFP          ; Save FPREG on stack
	ld      BC,8420H        ; BCDE = 10.
	ld      DE,0000H
	call    FPBCDE          ; Move 10 to FPREG

DIV:    pop     BC              ; Get number from stack
	pop     DE
DVBCDE: call    TSTSGN          ; Test sign of FPREG
	jp      Z,DZERR         ; Error if division by zero
	ld      L,-1            ; Flag subtract exponents
	call    ADDEXP          ; Subtract exponents
	inc     (HL)            ; Add 2 to exponent to adjust
	inc     (HL)
	dec     HL              ; Point to MSB
	ld      A,(HL)          ; Get MSB of dividend
	ld      (DIV3),A        ; Save for subtraction
	dec     HL
	ld      A,(HL)          ; Get NMSB of dividend
	ld      (DIV2),A        ; Save for subtraction
	dec     HL
	ld      A,(HL)          ; Get MSB of dividend
	ld      (DIV1),A        ; Save for subtraction
	ld      B,C             ; Get MSB
	ex      DE,HL           ; NMSB,LSB to HL
	xor      A
	ld      C,A             ; Clear MSB of quotient
	ld      D,A             ; Clear NMSB of quotient
	ld      E,A             ; Clear LSB of quotient
	ld      (DIV4),A        ; Clear overflow count
DIVLP:  push    HL              ; Save divisor
	push    BC
	ld      A,L             ; Get LSB of number
	call    DIVSUP          ; Subt' divisor from dividend
	sbc     A,0             ; Count for overflows
	CCF
	jr      NC,RESDIV       ; Restore divisor if borrow
	ld      (DIV4),A        ; Re-save overflow count
	pop     AF              ; Scrap divisor
	pop     AF
	SCF                     ; Set carry to
	DB      0D2H            ; Skip "pop BC" and "pop HL"

RESDIV: pop     BC              ; Restore divisor
	pop     HL
	ld      A,C             ; Get MSB of quotient
	inc     A
	dec     A
	rra                     ; Bit 0 to bit 7
	jp      M,RONDB         ; Done - Normalise result
	rla                     ; Restore carry
	ld      A,E             ; Get LSB of quotient
	rla                     ; Double it
	ld      E,A             ; Put it back
	ld      A,D             ; Get NMSB of quotient
	rla                     ; Double it
	ld      D,A             ; Put it back
	ld      A,C             ; Get MSB of quotient
	rla                     ; Double it
	ld      C,A             ; Put it back
	add     HL,HL           ; Double NMSB,LSB of divisor
	ld      A,B             ; Get MSB of divisor
	rla                     ; Double it
	ld      B,A             ; Put it back
	ld      A,(DIV4)        ; Get VLSB of quotient
	rla                     ; Double it
	ld      (DIV4),A        ; Put it back
	ld      A,C             ; Get MSB of quotient
	or      D               ; Merge NMSB
	or      E               ; Merge LSB
	jp      NZ,DIVLP        ; Not done - Keep dividing
	push    HL              ; Save divisor
	ld      HL,FPEXP        ; Point to exponent
	dec     (HL)            ; Divide by 2
	pop     HL              ; Restore divisor
	jp      NZ,DIVLP        ; Ok - Keep going
	jp      OVERR           ; Overflow error

ADDEXP: ld      A,B             ; Get exponent of dividend
	or      A               ; Test it
	jp      Z,OVTST3        ; Zero - Result zero
	ld      A,L             ; Get add/subtract flag
	ld      HL,FPEXP        ; Point to exponent
	xor      (HL)            ; Add or subtract it
	add     A,B             ; Add the other exponent
	ld      B,A             ; Save new exponent
	rra                     ; Test exponent for overflow
	xor      B
	ld      A,B             ; Get exponent
	jp      P,OVTST2        ; Positive - Test for overflow
	add     A,80H	; Add excess 128
	ld      (HL),A          ; Save new exponent
	jp      Z,POPHRT        ; Zero - Result zero
	call    SIGNS           ; Set MSBs and sign of result
	ld      (HL),A          ; Save new exponent
	dec     HL              ; Point to MSB
	ret

OVTST1: call    TSTSGN          ; Test sign of FPREG
	cpl                     ; Invert sign
	pop     HL              ; Clean up stack
OVTST2: or      A               ; Test if new exponent zero
OVTST3: pop     HL              ; Clear off return address
	jp      P,RESZER        ; Result zero
	jp      OVERR           ; Overflow error

MLSP10: call    BCDEFP          ; Move FPREG to BCDE
	ld      A,B             ; Get exponent
	or      A               ; Is it zero?
	ret     Z               ; Yes - Result is zero
	add     A,2             ; Multiply by 4
	jp      C,OVERR         ; Overflow - ?OV Error
	ld      B,A             ; Re-save exponent
	call    FPADD           ; Add BCDE to FPREG (Times 5)
	ld      HL,FPEXP        ; Point to exponent
	inc     (HL)            ; Double number (Times 10)
	ret     NZ              ; Ok - Return
	jp      OVERR           ; Overflow error

TSTSGN: ld      A,(FPEXP)       ; Get sign of FPREG
	or      A
	ret     Z               ; return if number is zero
	ld      A,(FPREG+2)     ; Get MSB of FPREG
	DB      0FEH            ; Test sign
RETREL: cpl                     ; Invert sign
	rla                     ; Sign bit to carry
FLGDIF: sbc     A,A             ; Carry to all bits of A
	ret     NZ              ; Return -1 if negative
	inc     A               ; Bump to +1
	ret                     ; Positive - Return +1

SGN:    call    TSTSGN          ; Test sign of FPREG
FLGREL: ld      B,80H+8         ; 8 bit integer in exponent
	ld      DE,0            ; Zero NMSB and LSB
RETINT: ld      HL,FPEXP        ; Point to exponent
	ld      C,A             ; CDE = MSB,NMSB and LSB
	ld      (HL),B          ; Save exponent
	ld      B,0             ; CDE = integer to normalise
	inc     HL              ; Point to sign of result
	ld      (HL),80H        ; Set sign of result
	rla                     ; Carry = sign of integer
	jp      CONPOS          ; Set sign of result

ABS:    call    TSTSGN          ; Test sign of FPREG
	ret     P               ; Return if positive
INVSGN: ld      HL,FPREG+2      ; Point to MSB
	ld      A,(HL)          ; Get sign of mantissa
	xor      80H             ; Invert sign of mantissa
	ld      (HL),A          ; Re-save sign of mantissa
	ret

STAKFP: ex      DE,HL           ; Save code string address
	ld      HL,(FPREG)      ; LSB,NLSB of FPREG
	ex      (SP),HL         ; Stack them,get return
	push    HL              ; Re-save return
	ld      HL,(FPREG+2)    ; MSB and exponent of FPREG
	ex      (SP),HL         ; Stack them,get return
	push    HL              ; Re-save return
	ex      DE,HL           ; Restore code string address
	ret

PHLTFP: call    LOADFP          ; Number at HL to BCDE
FPBCDE: ex      DE,HL           ; Save code string address
	ld      (FPREG),HL      ; Save LSB,NLSB of number
	ld      H,B             ; Exponent of number
	ld      L,C             ; MSB of number
	ld      (FPREG+2),HL    ; Save MSB and exponent
	ex      DE,HL           ; Restore code string address
	ret

BCDEFP: ld      HL,FPREG        ; Point to FPREG
LOADFP: ld      E,(HL)          ; Get LSB of number
	inc     HL
	ld      D,(HL)          ; Get NMSB of number
	inc     HL
	ld      C,(HL)          ; Get MSB of number
	inc     HL
	ld      B,(HL)          ; Get exponent of number
INCHL:  inc     HL              ; Used for conditional "inc HL"
	ret

FPTHL:  ld      DE,FPREG        ; Point to FPREG
DETHL4: ld      B,4             ; 4 DBs to move
DETHLB: ld      A,(DE)          ; Get source
	ld      (HL),A          ; Save destination
	inc     DE              ; Next source
	inc     HL              ; Next destination
	dec     B               ; Count DBs
	jr      NZ,DETHLB       ; Loop if more
	ret

SIGNS:  ld      HL,FPREG+2      ; Point to MSB of FPREG
	ld      A,(HL)          ; Get MSB
	rlca                    ; Old sign to carry
	SCF                     ; Set MSBit
	rra                     ; Set MSBit of MSB
	ld      (HL),A          ; Save new MSB
	CCF                     ; Complement sign
	rra                     ; Old sign to carry
	inc     HL
	inc     HL
	ld      (HL),A          ; Set sign of result
	ld      A,C             ; Get MSB
	rlca                    ; Old sign to carry
	SCF                     ; Set MSBit
	rra                     ; Set MSBit of MSB
	ld      C,A             ; Save MSB
	rra
	xor      (HL)            ; New sign of result
	ret

CMPNUM: ld      A,B             ; Get exponent of number
	or      A
	jp      Z,TSTSGN        ; Zero - Test sign of FPREG
	ld      HL,RETREL       ; Return relation routine
	push    HL              ; Save for return
	call    TSTSGN          ; Test sign of FPREG
	ld      A,C             ; Get MSB of number
	ret     Z               ; FPREG zero - Number's MSB
	ld      HL,FPREG+2      ; MSB of FPREG
	xor      (HL)            ; Combine signs
	ld      A,C             ; Get MSB of number
	ret     M               ; Exit if signs different
	call    CMPFP           ; Compare FP numbers
	rra                     ; Get carry to sign
	xor      C               ; Combine with MSB of number
	ret

CMPFP:  inc     HL              ; Point to exponent
	ld      A,B             ; Get exponent
	cp      (HL)            ; Compare exponents
	ret     NZ              ; Different
	dec     HL              ; Point to MBS
	ld      A,C             ; Get MSB
	cp      (HL)            ; Compare MSBs
	ret     NZ              ; Different
	dec     HL              ; Point to NMSB
	ld      A,D             ; Get NMSB
	cp      (HL)            ; Compare NMSBs
	ret     NZ              ; Different
	dec     HL              ; Point to LSB
	ld      A,E             ; Get LSB
	sub     (HL)            ; Compare LSBs
	ret     NZ              ; Different
	pop     HL              ; Drop return
	pop     HL              ; Drop another return
	ret

FPINT:  ld      B,A             ; <- Move
	ld      C,A             ; <- exponent
	ld      D,A             ; <- to all
	ld      E,A             ; <- bits
	or      A               ; Test exponent
	ret     Z               ; Zero - Return zero
	push    HL              ; Save pointer to number
	call    BCDEFP          ; Move FPREG to BCDE
	call    SIGNS           ; Set MSBs & sign of result
	xor      (HL)            ; Combine with sign of FPREG
	ld      H,A             ; Save combined signs
	call    M,DCBCDE        ; Negative - Decrement BCDE
	ld      A,80H+24        ; 24 bits
	sub     B               ; Bits to shift
	call    SCALE           ; Shift BCDE
	ld      A,H             ; Get combined sign
	rla                     ; Sign to carry
	call    C,FPROND        ; Negative - Round number up
	ld      B,0             ; Zero exponent
	call    C,COMPL         ; If negative make positive
	pop     HL              ; Restore pointer to number
	ret

DCBCDE: dec     DE              ; Decrement BCDE
	ld      A,D             ; Test LSBs
	and     E
	inc     A
	ret     NZ              ; Exit if LSBs not FFFF
	dec     BC              ; Decrement MSBs
	ret

INT:    ld      HL,FPEXP        ; Point to exponent
	ld      A,(HL)          ; Get exponent
	cp      80H+24          ; Integer accuracy only?
	ld      A,(FPREG)       ; Get LSB
	ret     NC              ; Yes - Already integer
	ld      A,(HL)          ; Get exponent
	call    FPINT           ; F.P to integer
	ld      (HL),80H+24     ; Save 24 bit integer
	ld      A,E             ; Get LSB of number
	push    AF              ; Save LSB
	ld      A,C             ; Get MSB of number
	rla                     ; Sign to carry
	call    CONPOS          ; Set sign of result
	pop     AF              ; Restore LSB of number
	ret

MLDEBC: ld      HL,0            ; Clear partial product
	ld      A,B             ; Test multiplier
	or      C
	ret     Z               ; Return zero if zero
	ld      A,16            ; 16 bits
MLDBLP: add     HL,HL           ; Shift P.P left
	jp      C,BSERR         ; ?BS Error if overflow
	ex      DE,HL
	add     HL,HL           ; Shift multiplier left
	ex      DE,HL
	jr      NC,NOMLAD       ; Bit was zero - No add
	add     HL,BC           ; Add multiplicand
	jp      C,BSERR         ; ?BS Error if overflow
NOMLAD: dec     A               ; Count bits
	jr      NZ,MLDBLP       ; More
	ret

ASCTFP: cp      '-'             ; Negative?
	push    AF              ; Save it and flags
	jr      Z,CNVNUM        ; Yes - Convert number
	cp      '+'             ; Positive?
	jr      Z,CNVNUM        ; Yes - Convert number
	dec     HL              ; dec 'cos GETCHR incs
CNVNUM: call    RESZER          ; Set result to zero
	ld      B,A             ; Digits after point counter
	ld      D,A             ; Sign of exponent
	ld      E,A             ; Exponent of ten
	cpl
	ld      C,A             ; Before or after point flag
MANLP:  call    GETCHR          ; Get next character
	jp      C,ADDIG         ; Digit - Add to number
	cp      '.'
	jr      Z,DPOINT        ; '.' - Flag point
	cp      'E'
	jr      NZ,CONEXP       ; Not 'E' - Scale number
	call    GETCHR          ; Get next character
	call    SGNEXP          ; Get sign of exponent
EXPLP:  call    GETCHR          ; Get next character
	jp      C,EDIGIT        ; Digit - Add to exponent
	inc     D               ; Is sign negative?
	jr      NZ,CONEXP       ; No - Scale number
	xor      A
	sub     E               ; Negate exponent
	ld      E,A             ; And re-save it
	inc     C               ; Flag end of number
DPOINT: inc     C               ; Flag point passed
	jr      Z,MANLP         ; Zero - Get another digit
CONEXP: push    HL              ; Save code string address
	ld      A,E             ; Get exponent
	sub     B               ; Subtract digits after point
SCALMI: call    P,SCALPL        ; Positive - Multiply number
	jp      P,ENDCON        ; Positive - All done
	push    AF              ; Save number of times to /10
	call    DIV10           ; Divide by 10
	pop     AF              ; Restore count
	inc     A               ; Count divides

ENDCON: jr      NZ,SCALMI       ; More to do
	pop     DE              ; Restore code string address
	pop     AF              ; Restore sign of number
	call    Z,INVSGN        ; Negative - Negate number
	ex      DE,HL           ; Code string address to HL
	ret

SCALPL: ret     Z               ; Exit if no scaling needed
MULTEN: push    AF              ; Save count
	call    MLSP10          ; Multiply number by 10
	pop     AF              ; Restore count
	dec     A               ; Count multiplies
	ret

ADDIG:  push    DE              ; Save sign of exponent
	ld      D,A             ; Save digit
	ld      A,B             ; Get digits after point
	adc     A,C             ; Add one if after point
	ld      B,A             ; Re-save counter
	push    BC              ; Save point flags
	push    HL              ; Save code string address
	push    DE              ; Save digit
	call    MLSP10          ; Multiply number by 10
	pop     AF              ; Restore digit
	sub     '0'             ; Make it absolute
	call    RSCALE          ; Re-scale number
	pop     HL              ; Restore code string address
	pop     BC              ; Restore point flags
	pop     DE              ; Restore sign of exponent
	jp      MANLP           ; Get another digit

RSCALE: call    STAKFP          ; Put number on stack
	call    FLGREL          ; Digit to add to FPREG
PADD:   pop     BC              ; Restore number
	pop     DE
	jp      FPADD           ; Add BCDE to FPREG and return

EDIGIT: ld      A,E             ; Get digit
	rlca                    ; Times 2
	rlca                    ; Times 4
	add     A,E             ; Times 5
	rlca                    ; Times 10
	add     A,(HL)          ; Add next digit
	sub     '0'             ; Make it absolute
	ld      E,A             ; Save new digit
	jp      EXPLP           ; Look for another digit

LINEIN: push    HL              ; Save code string address
	ld      HL,INMSG        ; Output " in "
	call    PRS             ; Output string at HL
	pop     HL              ; Restore code string address
PRNTHL: ex      DE,HL           ; Code string address to DE
	xor      A
	ld      B,80H+24        ; 24 bits
	call    RETINT          ; Return the integer
	ld      HL,PRNUMS       ; Print number string
	push    HL              ; Save for return
NUMASC: ld      HL,PBUFF        ; Convert number to ASCII
	push    HL              ; Save for return
	call    TSTSGN          ; Test sign of FPREG
	ld      (HL),' '        ; Space at start
	jp      P,SPCFST        ; Positive - Space to start
	ld      (HL),'-'        ; '-' sign at start
SPCFST: inc     HL              ; First DB of number
	ld      (HL),'0'        ; '0' if zero
	jp      Z,JSTZER        ; Return '0' if zero
	push    HL              ; Save buffer address
	call    M,INVSGN        ; Negate FPREG if negative
	xor      A               ; Zero A
	push    AF              ; Save it
	call    RNGTST          ; Test number is in range
SIXDIG: ld      BC,9143H        ; BCDE - 99999.9
	ld      DE,4FF8H
	call    CMPNUM          ; Compare numbers
	or      A
	jp      PO,INRNG        ; > 99999.9 - Sort it out
	pop     AF              ; Restore count
	call    MULTEN          ; Multiply by ten
	push    AF              ; Re-save count
	jr      SIXDIG          ; Test it again

GTSIXD: call    DIV10           ; Divide by 10
	pop     AF              ; Get count
	inc     A               ; Count divides
	push    AF              ; Re-save count
	call    RNGTST          ; Test number is in range
INRNG:  call    ROUND           ; Add 0.5 to FPREG
	inc     A
	call    FPINT           ; F.P to integer
	call    FPBCDE          ; Move BCDE to FPREG
	ld      BC,0306H        ; 1E+06 to 1E-03 range
	pop     AF              ; Restore count
	add     A,C             ; 6 digits before point
	inc     A               ; Add one
	jp      M,MAKNUM        ; Do it in 'E' form if < 1E-02
	cp      6+1+1           ; More than 999999 ?
	jr      NC,MAKNUM       ; Yes - Do it in 'E' form
	inc     A               ; Adjust for exponent
	ld      B,A             ; Exponent of number
	ld      A,2             ; Make it zero after

MAKNUM: dec     A               ; Adjust for digits to do
	dec     A
	pop     HL              ; Restore buffer address
	push    AF              ; Save count
	ld      DE,POWERS       ; Powers of ten
	dec     B               ; Count digits before point
	jp      NZ,DIGTXT       ; Not zero - Do number
	ld      (HL),'.'        ; Save point
	inc     HL              ; Move on
	ld      (HL),'0'        ; Save zero
	inc     HL              ; Move on
DIGTXT: dec     B               ; Count digits before point
	ld      (HL),'.'        ; Save point in case
	call    Z,INCHL         ; Last digit - move on
	push    BC              ; Save digits before point
	push    HL              ; Save buffer address
	push    DE              ; Save powers of ten
	call    BCDEFP          ; Move FPREG to BCDE
	pop     HL              ; Powers of ten table
	ld      B, '0'-1        ; ASCII '0' - 1
TRYAGN: inc     B               ; Count subtractions
	ld      A,E             ; Get LSB
	sub     (HL)            ; Subtract LSB
	ld      E,A             ; Save LSB
	inc     HL
	ld      A,D             ; Get NMSB
	sbc     A,(HL)          ; Subtract NMSB
	ld      D,A             ; Save NMSB
	inc     HL
	ld      A,C             ; Get MSB
	sbc     A,(HL)          ; Subtract MSB
	ld      C,A             ; Save MSB
	dec     HL              ; Point back to start
	dec     HL
	jp      NC,TRYAGN       ; No overflow - Try again
	call    PLUCDE          ; Restore number
	inc     HL              ; Start of next number
	call    FPBCDE          ; Move BCDE to FPREG
	ex      DE,HL           ; Save point in table
	pop     HL              ; Restore buffer address
	ld      (HL),B          ; Save digit in buffer
	inc     HL              ; And move on
	pop     BC              ; Restore digit count
	dec     C               ; Count digits
	jp      NZ,DIGTXT       ; More - Do them
	dec     B               ; Any decimal part?
	jr      Z,DOEBIT        ; No - Do 'E' bit
SUPTLZ: dec     HL              ; Move back through buffer
	ld      A,(HL)          ; Get character
	cp      '0'             ; '0' character?
	jr      Z,SUPTLZ        ; Yes - Look back for more
	cp      '.'             ; A decimal point?
	call    NZ,INCHL        ; Move back over digit

DOEBIT: pop     AF              ; Get 'E' flag
	jp      Z,NOENED        ; No 'E' needed - End buffer
	ld      (HL),'E'        ; Put 'E' in buffer
	inc     HL              ; And move on
	ld      (HL),'+'        ; Put '+' in buffer
	jp      P,OUTEXP        ; Positive - Output exponent
	ld      (HL),'-'        ; Put '-' in buffer
	cpl                     ; Negate exponent
	inc     A
OUTEXP: ld      B,'0'-1         ; ASCII '0' - 1
EXPTEN: inc     B               ; Count subtractions
	sub     10              ; Tens digit
	jr      NC,EXPTEN       ; More to do
	add     A,'0'+10        ; Restore and make ASCII
	inc     HL              ; Move on
	ld      (HL),B          ; Save MSB of exponent
JSTZER: inc     HL              ;
	ld      (HL),A          ; Save LSB of exponent
	inc     HL
NOENED: ld      (HL),C          ; Mark end of buffer
	pop     HL              ; Restore code string address
	ret

RNGTST: ld      BC,9474H        ; BCDE = 999999.
	ld      DE,23F7H
	call    CMPNUM          ; Compare numbers
	or      A
	pop     HL              ; Return address to HL
	jp      PO,GTSIXD       ; Too big - Divide by ten
	jp      (HL)            ; Otherwise return to caller

HALF:   DB      00H,00H,00H,80H ; 0.5

POWERS: DB      0A0H,086H,001H  ; 100000
	DB      010H,027H,000H  ;  10000
	DB      0E8H,003H,000H  ;   1000
	DB      064H,000H,000H  ;    100
	DB      00AH,000H,000H  ;     10
	DB      001H,000H,000H  ;      1

NEGAFT: ld  HL,INVSGN           ; Negate result
	ex      (SP),HL         ; To be done after caller
	jp      (HL)            ; Return to caller

SQR:    call    STAKFP          ; Put value on stack
	ld      HL,HALF         ; Set power to 1/2
	call    PHLTFP          ; Move 1/2 to FPREG

POWER:  pop     BC              ; Get base
	pop     DE
	call    TSTSGN          ; Test sign of power
	ld      A,B             ; Get exponent of base
	jp      Z,EXP           ; Make result 1 if zero
	jp      P,POWER1        ; Positive base - Ok
	or      A               ; Zero to negative power?
	jp      Z,DZERR         ; Yes - ?/0 Error
POWER1: or      A               ; Base zero?
	jp      Z,SAVEXP        ; Yes - Return zero
	push    DE              ; Save base
	push    BC
	ld      A,C             ; Get MSB of base
	or      01111111B       ; Get sign status
	call    BCDEFP          ; Move power to BCDE
	jp      P,POWER2        ; Positive base - Ok
	push    DE              ; Save power
	push    BC
	call    INT             ; Get integer of power
	pop     BC              ; Restore power
	pop     DE
	push    AF              ; MSB of base
	call    CMPNUM          ; Power an integer?
	pop     HL              ; Restore MSB of base
	ld      A,H             ; but don't affect flags
	rra                     ; Exponent odd or even?
POWER2: pop     HL              ; Restore MSB and exponent
	ld      (FPREG+2),HL    ; Save base in FPREG
	pop     HL              ; LSBs of base
	ld      (FPREG),HL      ; Save in FPREG
	call    C,NEGAFT        ; Odd power - Negate result
	call    Z,INVSGN        ; Negative base - Negate it
	push    DE              ; Save power
	push    BC
	call    LOG             ; Get LOG of base
	pop     BC              ; Restore power
	pop     DE
	call    FPMULT          ; Multiply LOG by power

EXP:    call    STAKFP          ; Put value on stack
	ld      BC,08138H       ; BCDE = 1/Ln(2)
	ld      DE,0AA3BH
	call    FPMULT          ; Multiply value by 1/LN(2)
	ld      A,(FPEXP)       ; Get exponent
	cp      80H+8           ; Is it in range?
	jp      NC,OVTST1       ; No - Test for overflow
	call    INT             ; Get INT of FPREG
	add     A,80H	; FOR excess 128
	add     A,2             ; Exponent > 126?
	jp      C,OVTST1        ; Yes - Test for overflow
	push    AF              ; Save scaling factor
	ld      HL,UNITY        ; Point to 1.
	call    ADDPHL          ; Add 1 to FPREG
	call    MULLN2          ; Multiply by LN(2)
	pop     AF              ; Restore scaling factor
	pop     BC              ; Restore exponent
	pop     DE
	push    AF              ; Save scaling factor
	call    SUBCDE          ; Subtract exponent from FPREG
	call    INVSGN          ; Negate result
	ld      HL,EXPTAB       ; Coefficient table
	call    SMSER1          ; Sum the series
	ld      DE,0            ; Zero LSBs
	pop     BC              ; Scaling factor
	ld      C,D             ; Zero MSB
	jp      FPMULT          ; Scale result to correct value

EXPTAB: DB      8                       ; Table used by EXP
	DB      040H,02EH,094H,074H     ; -1/7! (-1/5040)
	DB      070H,04FH,02EH,077H     ;  1/6! ( 1/720)
	DB      06EH,002H,088H,07AH     ; -1/5! (-1/120)
	DB      0E6H,0A0H,02AH,07CH     ;  1/4! ( 1/24)
	DB      050H,0AAH,0AAH,07EH     ; -1/3! (-1/6)
	DB      0FFH,0FFH,07FH,07FH     ;  1/2! ( 1/2)
	DB      000H,000H,080H,081H     ; -1/1! (-1/1)
	DB      000H,000H,000H,081H     ;  1/0! ( 1/1)

SUMSER: call    STAKFP          ; Put FPREG on stack
	ld      DE,MULT         ; Multiply by "X"
	push    DE              ; To be done after
	push    HL              ; Save address of table
	call    BCDEFP          ; Move FPREG to BCDE
	call    FPMULT          ; Square the value
	pop     HL              ; Restore address of table
SMSER1: call    STAKFP          ; Put value on stack
	ld      A,(HL)          ; Get number of coefficients
	inc     HL              ; Point to start of table
	call    PHLTFP          ; Move coefficient to FPREG
	DB      06H             ; Skip "pop AF"
SUMLP:  pop     AF              ; Restore count
	pop     BC              ; Restore number
	pop     DE
	dec     A               ; Cont coefficients
	ret     Z               ; All done
	push    DE              ; Save number
	push    BC
	push    AF              ; Save count
	push    HL              ; Save address in table
	call    FPMULT          ; Multiply FPREG by BCDE
	pop     HL              ; Restore address in table
	call    LOADFP          ; Number at HL to BCDE
	push    HL              ; Save address in table
	call    FPADD           ; Add coefficient to FPREG
	pop     HL              ; Restore address in table
	jr      SUMLP           ; More coefficients

RND:    call    TSTSGN          ; Test sign of FPREG
	ld      HL,SEED+2       ; Random number seed
	jp      M,RESEED        ; Negative - Re-seed
	ld      HL,LSTRND       ; Last random number
	call    PHLTFP          ; Move last RND to FPREG
	ld      HL,SEED+2       ; Random number seed
	ret     Z               ; Return if RND(0)
	add     A,(HL)          ; Add (SEED)+2)
	and     00000111B       ; 0 to 7
	ld      B,0
	ld      (HL),A          ; Re-save seed
	inc     HL              ; Move to coefficient table
	add     A,A             ; 4 DBs
	add     A,A             ; per entry
	ld      C,A             ; BC = Offset into table
	add     HL,BC           ; Point to coefficient
	call    LOADFP          ; Coefficient to BCDE
	call    FPMULT  ;       ; Multiply FPREG by coefficient
	ld      A,(SEED+1)      ; Get (SEED+1)
	inc     A               ; Add 1
	and     00000011B       ; 0 to 3
	ld      B,0
	cp      1               ; Is it zero?
	adc     A,B             ; Yes - Make it 1
	ld      (SEED+1),A      ; Re-save seed
	ld      HL,RNDTAB-4     ; Addition table
	add     A,A             ; 4 DBs
	add     A,A             ; per entry
	ld      C,A             ; BC = Offset into table
	add     HL,BC           ; Point to value
	call    ADDPHL          ; Add value to FPREG
RND1:   call    BCDEFP          ; Move FPREG to BCDE
	ld      A,E             ; Get LSB
	ld      E,C             ; LSB = MSB
	xor      01001111B       ; Fiddle around
	ld      C,A             ; New MSB
	ld      (HL),80H        ; Set exponent
	dec     HL              ; Point to MSB
	ld      B,(HL)          ; Get MSB
	ld      (HL),80H        ; Make value -0.5
	ld      HL,SEED         ; Random number seed
	inc     (HL)            ; Count seed
	ld      A,(HL)          ; Get seed
	sub     171             ; Do it modulo 171
	jr      NZ,RND2         ; Non-zero - Ok
	ld      (HL),A          ; Zero seed
	inc     C               ; Fillde about
	dec     D               ; with the
	inc     E               ; number
RND2:   call    BNORM           ; Normalise number
	ld      HL,LSTRND       ; Save random number
	jp      FPTHL           ; Move FPREG to last and return

RESEED: ld      (HL),A          ; Re-seed random numbers
	dec     HL
	ld      (HL),A
	dec     HL
	ld      (HL),A
	jr      RND1            ; Return RND seed

RNDTAB: DB   068H,0B1H,046H,068H     ; Table used by RND
	DB   099H,0E9H,092H,069H
	DB   010H,0D1H,075H,068H

COS:    ld      HL,HALFPI       ; Point to PI/2
	call    ADDPHL          ; Add it to PPREG
SIN:    call    STAKFP          ; Put angle on stack
	ld      BC,8349H        ; BCDE = 2 PI
	ld      DE,0FDBH
	call    FPBCDE          ; Move 2 PI to FPREG
	pop     BC              ; Restore angle
	pop     DE
	call    DVBCDE          ; Divide angle by 2 PI
	call    STAKFP          ; Put it on stack
	call    INT             ; Get INT of result
	pop     BC              ; Restore number
	pop     DE
	call    SUBCDE          ; Make it 0 <= value < 1
	ld      HL,QUARTR       ; Point to 0.25
	call    SUBPHL          ; Subtract value from 0.25
	call    TSTSGN          ; Test sign of value
	SCF                     ; Flag positive
	jp      P,SIN1          ; Positive - Ok
	call    ROUND           ; Add 0.5 to value
	call    TSTSGN          ; Test sign of value
	or      A               ; Flag negative
SIN1:   push    AF              ; Save sign
	call    P,INVSGN        ; Negate value if positive
	ld      HL,QUARTR       ; Point to 0.25
	call    ADDPHL          ; Add 0.25 to value
	pop     AF              ; Restore sign
	call    NC,INVSGN       ; Negative - Make positive
	ld      HL,SINTAB       ; Coefficient table
	jp      SUMSER          ; Evaluate sum of series

HALFPI: DB   0DBH,00FH,049H,081H     ; 1.5708 (PI/2)

QUARTR: DB   000H,000H,000H,07FH     ; 0.25

SINTAB: DB   5                       ; Table used by SIN
	DB   0BAH,0D7H,01EH,086H     ; 39.711
	DB   064H,026H,099H,087H     ;-76.575
	DB   058H,034H,023H,087H     ; 81.602
	DB   0E0H,05DH,0A5H,086H     ;-41.342
	DB   0DAH,00FH,049H,083H     ;  6.2832

TAN:    call    STAKFP          ; Put angle on stack
	call    SIN             ; Get SIN of angle
	pop     BC              ; Restore angle
	pop     HL
	call    STAKFP          ; Save SIN of angle
	ex      DE,HL           ; BCDE = Angle
	call    FPBCDE          ; Angle to FPREG
	call    COS             ; Get COS of angle
	jp      DIV             ; TAN = SIN / COS

ATN:    call    TSTSGN          ; Test sign of value
	call    M,NEGAFT        ; Negate result after if -ve
	call    M,INVSGN        ; Negate value if -ve
	ld      A,(FPEXP)       ; Get exponent
	cp      81H             ; Number less than 1?
	jp      C,ATN1          ; Yes - Get arc tangnt
	ld      BC,8100H        ; BCDE = 1
	ld      D,C
	ld      E,C
	call    DVBCDE          ; Get reciprocal of number
	ld      HL,SUBPHL       ; Sub angle from PI/2
	push    HL              ; Save for angle > 1
ATN1:   ld      HL,ATNTAB       ; Coefficient table
	call    SUMSER          ; Evaluate sum of series
	ld      HL,HALFPI       ; PI/2 - angle in case > 1
	ret                     ; Number > 1 - Sub from PI/2

ATNTAB: DB   9                       ; Table used by ATN
	DB   04AH,0D7H,03BH,078H     ; 1/17
	DB   002H,06EH,084H,07BH     ;-1/15
	DB   0FEH,0C1H,02FH,07CH     ; 1/13
	DB   074H,031H,09AH,07DH     ;-1/11
	DB   084H,03DH,05AH,07DH     ; 1/9
	DB   0C8H,07FH,091H,07EH     ;-1/7
	DB   0E4H,0BBH,04CH,07EH     ; 1/5
	DB   06CH,0AAH,0AAH,07FH     ;-1/3
	DB   000H,000H,000H,081H     ; 1/1


ARET:   ret                     ; A return instruction

GETINP: ;equ	_getkey_wait
	call TSTBRK
	cp 0ffh
	jr z, GETINP
	ret

CLS: 
	ld      A,CS            ; ASCII Clear screen
	jp      MONOUT          ; Output character

WIDTH:  call    GETINT          ; Get integer 0-255
	ld      A,E             ; Width to A
	ld      (LWIDTH),A      ; Set width
	ret

LINES:  call    GETNUM          ; Get a number
	call    DEINT           ; Get integer -32768 to 32767
	ld      (LINESC),DE     ; Set lines counter
	ld      (LINESN),DE     ; Set lines number
	ret

POINT:  call    DEINT           ; Get integer -32768 to 32767
	ld      B,0             ; Dummy Function
	ld      A,0             ; Always returns 0
	jp      ABPASS          ; Return integer AB

DEEK:   call    DEINT           ; Get integer -32768 to 32767
	push    DE              ; Save number
	pop     HL              ; Number to HL
	ld      B,(HL)          ; Get LSB of contents
	inc     HL
	ld      A,(HL)          ; Get MSB of contents
	jp      ABPASS          ; Return integer AB

DOKE:   call    GETNUM          ; Get a number
	call    DEINT           ; Get integer -32768 to 32767
	push    DE              ; Save address
	call    CHKSYN          ; Make sure ',' follows
	DB      ','
	call    GETNUM          ; Get a number
	call    DEINT           ; Get integer -32768 to 32767
	ex      (SP),HL         ; Save value,get address
	ld      (HL),E          ; Save LSB of value
	inc     HL
	ld      (HL),D          ; Save MSB of value
	pop     HL              ; Restore code string address
	ret

C_A_L_L:call    GETNUM          ; Get a number
	call    DEINT           ; Get integer -32768 to 32767
	push    DE              ; Save address
	ret

; HEX$(nn) Convert 16 bit number to HEXadecimal string

HEX: 	call	TSTNUM          ; Verify it's a number
	call	DEINT           ; Get integer -32768 to 32767
	push	BC              ; Save contents of BC
	ld	HL,PBUFF
	ld	A,D             ; Get high order into A
	cp      $0
	jr      Z,HEX2          ; Skip output if both high digits are zero
	call    BYT2ASC         ; Convert D to ASCII
	ld      A,B
	cp      '0'
	jr      Z,HEX1          ; Don't store high digit if zero
	ld	(HL),B          ; Store it to PBUFF
	inc	HL              ; Next location
HEX1:   ld	(HL),C          ; Store C to PBUFF+1
	inc     HL              ; Next location
HEX2:   ld	A,E             ; Get lower DB
	call    BYT2ASC         ; Convert E to ASCII
	ld      A,D
	cp      $0
	jr      NZ,HEX3         ; If upper DB was not zero then always print lower DB
	ld      A,B
	cp      '0'             ; If high digit of lower DB is zero then don't print
	jr      Z,HEX4
HEX3:   ld      (HL),B          ; to PBUFF+2
	inc     HL              ; Next location
HEX4:   ld      (HL),C          ; to PBUFF+3
	inc     HL              ; PBUFF+4 to zero
	xor      A               ; Terminating character
	ld      (HL),A          ; Store zero to terminate
	inc     HL              ; Make sure PBUFF is terminated
	ld      (HL),A          ; Store the double zero there
	pop     BC              ; Get BC back
	ld      HL,PBUFF        ; Reset to start of PBUFF
	jp      STR1            ; Convert the PBUFF to a string and return it

BYT2ASC:	ld      B,A             ; Save original value
	and     $0F             ; Strip off upper nybble
	cp      $0A             ; 0-9?
	jr      C,ADD30         ; If A-F, add 7 more
	add     A,$07           ; Bring value up to ASCII A-F
ADD30:	add     A,$30           ; And make ASCII
	ld      C,A             ; Save converted char to C
	ld      A,B             ; Retrieve original value
	rrca                    ; and Rotate it right
	rrca
	rrca
	rrca
	and     $0F             ; Mask off upper nybble
	cp      $0A             ; 0-9? < A hex?
	jr      C,ADD301        ; Skip Add 7
	add     A,$07           ; Bring it up to ASCII A-F
ADD301:	add     A,$30           ; And make it full ASCII
	ld      B,A             ; Store high order DB
	ret	

; Convert "&Hnnnn" to FPREG
; Gets a character from (HL) checks for HEXadecimal ASCII numbers "&Hnnnn"
; Char is in A, NC if char is ;<=>?@ A-z, CY is set if 0-9
HEXTFP:  ex      DE,HL           ; Move code string pointer to DE
	ld      HL,$0000        ; Zero out the value
	call    GETHEX          ; Check the number for valid hex
	jp      C,HXERR         ; First value wasn't hex, HX error
	jr      HEXLP1          ; Convert first character
HEXLP:   call    GETHEX          ; Get second and addtional characters
	jr      C,HEXIT         ; Exit if not a hex character
HEXLP1:  add     HL,HL           ; Rotate 4 bits to the left
	add     HL,HL
	add     HL,HL
	add     HL,HL
	or      L               ; Add in D0-D3 into L
	ld      L,A             ; Save new value
	jr      HEXLP           ; And continue until all hex characters are in

GETHEX:  inc     DE              ; Next location
	ld      A,(DE)          ; Load character at pointer
	cp      ' '
	jr      Z,GETHEX        ; Skip spaces
	sub     $30             ; Get absolute value
	ret     C               ; < "0", error
	cp      $0A
	jr      C,NOSUB7        ; Is already in the range 0-9
	sub     $07             ; Reduce to A-F
	cp      $0A             ; Value should be $0A-$0F at this point
	ret     C               ; CY set if was :            ; < = > ? @
NOSUB7:  cp      $10             ; > Greater than "F"?
	CCF
	ret                     ; CY set if it wasn't valid hex
    
HEXIT:   ex      DE,HL           ; Value into DE, Code string into HL
	ld      A,D             ; Load DE into AC
	ld      C,E             ; FOR prep to 
	push    HL
	call    ACPASS          ; ACPASS to set AC as integer into FPREG
	pop     HL
	ret

HXERR:  ld      E,HX            ; ?HEX Error
	jp      ERROR

; BIN$(NN) Convert integer to a 1-16 char binary string
BIN:    call    TSTNUM          ; Verify it's a number
	call    DEINT           ; Get integer -32768 to 32767
BIN2:   push    BC              ; Save contents of BC
	ld      HL,PBUFF
	ld      B,17            ; One higher than max char count
ZEROSUP:                        ; Suppress leading zeros
	dec     B               ; Max 16 chars
	ld      A,B
	cp      $01
	jr      Z,BITOUT        ; Always output at least one character
	RL      E
	RL      D
	jr      NC,ZEROSUP
	jr      BITOUT2
BITOUT:      
	RL      E
	RL      D               ; Top bit now in carry
BITOUT2:
	ld      A,'0'           ; Char for '0'
	adc     A,0             ; If carry set then '0' --> '1'
	ld      (HL),A
	inc     HL
	dec     B
	jr      NZ,BITOUT
	xor      A               ; Terminating character
	ld      (HL),A          ; Store zero to terminate
	inc     HL              ; Make sure PBUFF is terminated
	ld      (HL),A          ; Store the double zero there
	pop     BC
	ld      HL,PBUFF
	jp      STR1

; Convert "&Bnnnn" to FPREG
; Gets a character from (HL) checks for Binary ASCII numbers "&Bnnnn"
BINTFP: ex      DE,HL           ; Move code string pointer to DE
	ld      HL,$0000        ; Zero out the value
	call    CHKBIN          ; Check the number for valid bin
	jr      C,BINERR        ; First value wasn't bin, HX error
BINIT:  sub     '0'
	add     HL,HL           ; Rotate HL left
	or      L
	ld      L,A
	call    CHKBIN          ; Get second and addtional characters
	jr      NC,BINIT        ; Process if a bin character
	ex      DE,HL           ; Value into DE, Code string into HL
	ld      A,D             ; Load DE into AC
	ld      C,E             ; FOR prep to 
	push    HL
	call    ACPASS          ; ACPASS to set AC as integer into FPREG
	pop     HL
	ret

; Char is in A, NC if char is 0 or 1
CHKBIN: inc     DE
	ld      A,(DE)
	cp      ' '
	jr      Z,CHKBIN        ; Skip spaces
	cp      '0'             ; Set C if < '0'
	ret     C
	cp      '2'
	CCF                     ; Set C if > '1'
	ret

BINERR: ld      E,BN            ; ?BIN Error
	jp      ERROR


JJUMP1: 
	ld      IX,-1           ; Flag cold start
	jr      CSTART          ; Go and initialise

MONOUT: equ	_writechar

MONITR: 
	jp      _return_to_rom	; Restart (Normally Monitor Start)

INITST: ld      A,0             ; Clear break flag
	ld      (BRKFLG),A
	jp      INIT

CSTART:	ld      HL,WRKSPC       ; Start of workspace RAM
	ld      SP,HL           ; Set up a temporary stack
	jr      INITST          ; Go to initialise

ARETN:  retN                    ; Return from NMI

TSTBIT: push    AF              ; Save bit mask
	and     B               ; Get common bits
	pop     BC              ; Restore bit mask
	cp      B               ; Same bit set?
	ld      A,0             ; Return 0 in A
	ret

OUTNCR: call    OUTC            ; Output character in A
	jp      PRNTCRLF        ; Output CRLF

_code_end:
;; End of Main Application

;; Fill to end of file
	org 0d0ffh
	seek 030ffh
	DB 000h
_file_end:
