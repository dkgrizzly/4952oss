; z80dasm 1.1.4
; command line: z80dasm -t -a -g 0xa000 -o COPIER.ASM COPIER.APP

	org	0a000h
	seek 00000h
	defb "4952 Protocol Analyzer"

	org 0a016h
	seek 00016h
	defw 003c4h
	defw 00800h

	org 0a01ah
	seek 0001ah
	defb "4952 Games Pack - TETRIS        "

_filesize:
	org 0a102h
	seek 00102h
	defw 00030h								; Blocks in file - 1

	defb " HP4952 TETRIS    4952  "
	defw 00800h
_fileflags:
	defw 00000h							; Flags 0200h is copy protect

	defb "4952 Games Pack - TETRIS        "

;; Entry Point
	org 0a147h
	seek 00147h
	defb 0000h
_entryaddr:
	defw __init
	
;; Main Application
	org 0a150h
	seek 00150h
	defw _play_game

;; ???
	org 0a17eh
	seek 0017eh
	defw 0f958h

;; Dynamic link loader data pointer & size
	org 0a180h
	seek 00180h
	defw 00014h			; Number of patches
	defw 0a25ah			; Location of patches

__init:
	di					; Disable Interrupts
	call _load_dll_stub	; Call our dynamic linker

	ld de,02000h		;   
	ld hl,0a800h		; Load menu data & stubs
	ld bc,00200h		;
	ldir				;

	jp 02000h			; Run main menu stub

__0a196h:
	ld hl,0a800h		;
	ld de,02000h		; Load menu data & stubs again?
	ld bc,00200h		;
	ldir				;

	ld hl,07621h		;
	push hl				;
	ld a,002h			; Patch function at 1109?
	ld (0110ch),a		;
	ld hl,0d966h		;
	ld (0110dh),hl		;
	call 01109h			;

__0a1b3h:
	ld hl,0a800h		;
	ld de,02000h		; Load menu data & stubs again?
	ld bc,00200h		;
	ldir				;

	ld hl,0761ch		;
	ex (sp), hl			;
	ld a,002h			; Patch function at 1109?
	ld (0110ch),a		;
	ld hl,0d966h		;
	ld (0110dh),hl		;
	call 01109h			;

	jp __0a1b3h			; Loop Forever
	ret					; How can we ever get here?

;; This is a dynamic linker, at runtime it loads a copy of the
;; ROM vector table into RAM and fixes up all the stubbed
;; ROM references in the executable

;; Load and execute ordinal patching stub from a safe location
_load_dll_stub:
	ld hl,0a210h		;
	ld de,02a00h		;
	ld bc,00036h		;
	ldir				;
	call 02a00h			;

	ld ix,(0a182h)		; Load patch table from ()
	ld bc,(0a180h)		; Load patch count
	ld l,(ix+000h)		;
	ld h,(ix+001h)		;
	ld e,(hl)			; Read L Byte
	inc hl				;
	ld d,(hl)			; Read H Byte
	ld l,(ix+002h)		; Get Patch Value
	ld h,(ix+003h)		;
	add hl,de			; Patch the pointer
	ex de,hl			;
	ld l,(ix+004h)		; Dest Address
	ld h,(ix+005h)		;
	ld (hl),e			; Write L Byte
	inc hl				;
	ld (hl),d			; Write H Byte
	ld de,00006h		;
	add ix,de			; Next Entry
	dec bc				;
	ld a,b				;
	or c				;
	jr nz,$-34			; More entries?
	ret					;

;; Local temp index variable
_dll_tmp:
	defb 000h

;; 54 Bytes - Relocated at runtime to 02a00h and executed
	org 02a00h
	seek 00210h

_dll_stub:
	ld a,004h			; Access Page 4
	out (020h),a		;
	ld hl,08000h		; Copy system ordinals from 10046 ROM
	ld de,02d00h		;
	ld bc,00134h		;
	ldir				;
	ld a,002h			; Access Page 2
	out (020h),a		;

	ld hl,(02d0ch)		; Generate 17 more for 02e34h = 0d9f0h...0da20h
	ld bc,00003h		;
	ld a,011h			; .. Source appears to be a jump table 
	call la246h			;

	ld hl,(02e16h)		; Generate 68 more for 02e56h = 
	ld a,(hl)			; 
	inc hl				; .. (some FM going on here...)
	ld h,(hl)			; 
	ld l,a				;
	ld bc,00006h		;
	ld a,044h			;
	call 0a246h			;

	ld bc,00002h		; Generate 30 more for 02edeh = 0eb98h..
	ld a,01eh			;
	call 0a246h			;
	ret					;

	org 0a246h
	seek 00246h
la246h:
	ld ix,_dll_tmp		;
	ld (ix+000h),a		;
	ld a,l				; do {
	ld (de),a			;   *DE = L
	inc de				;   DE++
	ld a,h				;
	ld (de),a			;   *DE = H
	inc de				;   DE++
	add hl,bc			;   HL+=BC
	dec (ix+000h)		; } while(TMP-- != 0)
	jr nz,$-10			; 
	ret					;

	org 0a25ah
	seek 0025ah
__dll_fixups:
	defw 02d32h, 00000h, 0a801h
	defw 02e6eh, 00000h, 0a804h
	defw 02d4ah, 00000h, 0a807h
	defw 02d50h, 00000h, 0a811h
	defw 02d6ch, 00000h, 0a818h
	defw 02d02h, 00000h, 0a81dh
	defw 02d02h, 00000h, 0a82dh
	defw 02e32h, 00000h, 0a830h
	defw 02d02h, 00000h, 0a835h
	defw 02e66h, 00003h, 0a868h
	defw 02e66h, 00004h, 0a86eh
	defw 02d02h, 00000h, 0a946h
	defw 02eceh, 00003h, 0a1a8h
	defw 02e54h, 00000h, 0a1abh
	defw 02eceh, 00004h, 0a1aeh
	defw 02eceh, 00000h, 0a1b1h
	defw 02eceh, 00003h, 0a1c5h
	defw 02e54h, 00000h, 0a1c8h
	defw 02eceh, 00004h, 0a1cbh
	defw 02eceh, 00000h, 0a1ceh


;; Relocated at runtime from 0a800h to 02000h
	org 02000h
	seek 00800h

	call 01543h			; Patched to 2d32 -> 01543h
	call 00fe9h			; Patched to 2e6e -> ????
	call 00085h			; Patched to 2d4a -> 00085h
	call l2065h			;
	ld hl,_splash_screen_data		;200c	21 71 20 	! q   
	push hl				;
	call 01cf8h			; Patched to 2d50 -> 01cf8h
	pop hl				;
	call l2032h			;
	call 0007eh			; Patched to 2d6c -> 0007eh

	ld a,006h			; Load Page 6 (Application RAM)
	call 00e60h			; Patched to 2d02 -> 00e60h

	ld de,0a800h		;
	ld hl,02000h		; Load this section over again (dangerous)
	ld bc,00200h		;
	ldir				;

	ld a,002h			; Load Page 2 (10046 ROM)
	call 00e60h			; Patched to 2d02 -> 00e60h

	jp 014d5h			; Return via call -> 02e32h -> 014d5h

l2032h:
	ld a,002h			; Load Page 2 (10046 ROM)
	call 00e60h			;
	ld hl,_splash_screen_data		;2037	21 71 20 	! q   
	ld (0761dh),hl		; Screen Paint Script Location
	ld hl,(0761fh)		; Copy main menu pointers
	ld de,_p_main_menu_page_one		; over the first page menu
	ld (0761fh),de		; pointers in our table
	ld (07624h),de		;
	ld bc,0000eh		;
	ldir				;

	ld a,(hl)			;
	inc hl				;
	ld h,(hl)			;
	ld l,a				;
	inc hl				; Skip to page two...
	inc hl				;
	ld de,_p_mm_reset	;
	ld bc,0000ch		;
	ldir				;

	ld hl,_play_game	; Patch our application vector
	ld (_p_mm_playgame),hl		; for button five on page two
	ret					;

l2065h:
	ld a,006h			; Patch 00fd4h -> our menu display function
	ld (00fd4h),a		;q
	ld hl,0a196h		; 
	ld (00fd5h),hl		;
	ret					;

	org 02071h
	seek 00871h
_splash_screen_data:
	defb 0ffh

	defb 002h, 003h, 0abh
	defb "    ", 000h 
	defb 003h, 005h, 0abh
	defb "    ", 000h 

	defb 002h, 00eh, 083h
	defb "TETRIS", 000h

	defb 002h, 01bh, 0abh
	defb "  ", 000h
	defb 003h, 019h, 0abh
	defb "      ", 000h

	defb 006h, 009h, 083h
	defb "for the HP 4952", 000h
	defb 007h, 008h, 083h
	defb "Protocol Analyzer", 000h

	defb 00ah, 008h, 083h, 0abh
	defb " Copyright 2018", 000h
	defb 00bh, 00ah, 083h
	defb "Grizzly Adams", 000h

	defb 000h							;; End of Screen Data

_splash_menu_data:
	defb "Re-!BERT!Remote!Mass !Play!Self~"
	defb "set!Menu!&Print!Store!Game!Test|"

_p_main_menu_page_one:
	defw 08336h			;; First Page Menu Data
_p_mm_autoconfig:
	defw 0141ch			;; Ordinal 120h Auto Config
_p_mm_setup:
	defw 0b5a8h			;; Entry Point for Setup
_p_mm_mon:
	defw 0100dh			;; Entry Point for Monitor Menu
_p_mm_sim:
	defw 01013h			;; Entry Point for Sim Menu
_p_mm_run:
	defw 0b9ffh			;; Entry Point for Run Menu
_p_mm_exam:
	defw 013cdh			;; Ordinal 12eh Examine Data
_p_mm_next1:
	defw _p_main_menu_page_two			;; Next Page

_p_main_menu_page_two:
	defw _splash_menu_data	;; Second Page Menu Data
_p_mm_reset:
	defw 0bb1ah			;; Entry Point for Re-Set
_p_mm_bert:
	defw 0b22ch			;; Entry Point for BERT Menu
_p_mm_remote:
	defw 0d963h			;; Entry Point for Remote & Print
_p_mm_masstorage:
	defw 00f0ch			;; Entry Point for Mass Storage
_p_mm_playgame:
	defw _play_game		;; Entry Point for Application
_p_mm_selftest:
	defw 0136fh			;; Ordinal 12ah Self Test
_p_mm_next2:
	defw _p_main_menu_page_one			;; Next Page

_play_game:
	ld a, 006h
	call 00e60h			; Page in 6
	ld hl,0aa00h		; Copy application to Work RAM
	ld de,02160h		;
	ld bc,01e9fh		; Fixme: update to size of main code
	ldir				;
	jp _game_main		; Run the application

;; End of menu section

;; Main Application
	org 2160h
	seek 0a00h

include "lib/screen.asm"
include "lib/keyb.asm"

_game_main:
	call _clear_screen

	ld a, 083h				; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 15
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a

;	jp 014d5h				; Return to main menu.

; TetrisJD V1.1 Build 20060507
; Par Jean-Denis Boivin (jeandenis.boivin@gmail.com)
; (c) 2006 Team-Atemis
; http://team-atemis.com

	LD		A, 1
	LD		(_cur_x), A
	LD		A, 1
	LD		(_cur_y), A
	LD		HL, STR_bienvenue_1
	call	_writestring

	LD		A, 1
	LD		(_cur_x), A
	LD		A, 2
	LD		(_cur_y), A
	LD		HL, STR_bienvenue_2
	call	_writestring

	LD		A, 1
	LD		(_cur_x), A
	LD		A, 3
	LD		(_cur_y), A
	LD		HL, STR_bienvenue_3
	call	_writestring

	LD		A, 1
	LD		(_cur_x), A
	LD		A, 4
	LD		(_cur_y), A
	LD		HL, STR_bienvenue_5
	call	_writestring

	LD		A, 1
	LD		(_cur_x), A
	LD		A, 5
	LD		(_cur_y), A
	LD		HL, STR_copyright_1
	call	_writestring

	call	_getkey_cooked

	call	_clear_screen
	CALL	CAL_draw_arena
	CALL	CAL_update_score
	LD		HL, INT_lvl
	LD		(HL), -1
	LD		HL, INT_speed_1
	LD		(HL), 25
	CALL	LBL_next_level
	CALL	CAL_random
	INC		HL					; +1
	ADD		HL, HL				; *2
	LD		D, H				; Save it
	LD		E, L
	ADD		HL, HL				; *4
	ADD		HL, HL				; *8
	ADD		HL, DE				; *10
	LD		D, H				; Save it
	LD		E, L
	LD		A, H				; Add upper & lower bytes together
	ADD		A, L
	LD		HL, INT_next_format
	LD		(HL), A
	CALL	CAL_draw_new_piece
	JP		LBL_game_loop
LBL_game_loop:
	CALL	CAL_getkey
	LD		A, (INT_quit)
	CP		2
	RET		Z
	LD		A, (INT_speed_1)
	LD		E, A
	LD		A, (INT_wait_moin_y)
	INC		A
	LD  	HL, INT_wait_moin_y
	LD		(HL), A
	CP		150
	CALL	Z, CAL_inc_wait_2
	LD		A, (INT_wait_moin_y_2)
	CP		E
	CALL	Z, LBL_game_move_down
	JP		LBL_game_loop
CAL_inc_wait_2:
	LD		HL, INT_wait_moin_y
	LD		(HL), 0
	LD		A, (INT_wait_moin_y_2)
	INC		A
	LD		HL, INT_wait_moin_y_2
	LD		(HL), A
	RET
CAL_getkey:
	call	_keyscan
	call	_getkey_raw
    CP		_key_rt
	JP		Z, LBL_game_move_right
	CP		_key_lt
	JP		Z, LBL_game_move_left
	CP		_key_up
	JP		Z, LBL_game_rotate
	CP		' '
	JP		Z, LBL_game_rotate_minus
	CP		_key_dn
	JP		Z, LBL_game_move_down
	CP		_key_exit
	JP		Z, LBL_quit
	CP		_key_more
	JP		Z, LBL_pause
	RET
CAL_wait_patiently:
	LD		E, 0
	LD		A, B
	CP		255
	RET		Z
	INC		A
	LD		B, A
	JP		CAL_wait_patiently2
CAL_wait_patiently2:
	LD		A, E
	CP		255
	JP		Z, CAL_wait_patiently
	INC		A
	LD		E, A
	JP		CAL_wait_patiently2
LBL_quit:
	LD		HL, INT_quit
	LD		(HL), 2
	CALL	CAL_dead_scrn_tetris
	CALL	CAL_dead_scrn_tetris2
	LD		B, 0
	CALL	CAL_wait_patiently
	call	_clear_screen
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 3
	LD		(_cur_y), A
	LD		HL, STR_bienvenue_3
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 4
	LD		(_cur_y), A
	LD		HL, STR_mort
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 5
	LD		(_cur_y), A
	LD		HL, ARR_score
	call	_writestring
;	JP		LBL_quit_2
LBL_quit_2:
	call	_keyscan
	call	_getkey_raw
    CP		00ah
	JP		Z, LBL_wtf
    JP		LBL_quit_2
LBL_wtf:
 	;LD 		HL,str1
	;RST 		20h
	;B_CALL(_ChkFindSym)
 	;LD 		HL,str1
	;RST 		20h
	;B_CALL(_chkFindSym)
	;PUSH 		DE
	;LD 		A,B
 	;POP 		DE
	;INC 		DE
	;INC 		DE
	;LD 		HL,ARR_score
	;LD 		BC,10
	;LDIR

	;LD		A, (INT_lvl)
	;B_CALL(_SetXXOP1)
	;B_CALL(_StoX)
	;LD		A, (INT_lvl2)
	;B_CALL(_SetXXOP1)
	;B_CALL(_StoR)
	jp 014d5h				; Return to HP main menu.
	;RET

;str1:
;	db StrngObj,tVarStrng,tStr1,$00

LBL_pause:
	call	_getkey_raw
    CP		_key_more
	JP		Z, CAL_getkey
	JP		LBL_pause

STR_mort:
	DB		":: GAME OVER ::", 0
INT_dead_x:
	DB		0
INT_dead_y:
	DB		0
CAL_dead_scrn_tetris2:
	RET
CAL_dead_scrn_tetris:
	LD		HL, INT_dead_x
	LD		(HL), 1
	LD		HL, INT_dead_y
	LD		(HL), 1
	JP		LBL_dead_scrn_tetris
LBL_dead_scrn_tetris:
	LD		A, (INT_dead_x)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_dead_y)
	SUB		4
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	LD		HL, INT_piece_skin
	LD		(HL), 1
	CALL	CAL_ppiece_off
	CALL	CAL_ppiece_on
	JP		LBL_dead_scrn_tetris_addx
LBL_dead_scrn_tetris_addx:
	LD		A, (INT_dead_x)
	INC		A
	LD		HL, INT_dead_x
	LD		(HL), A
	CP		12
	JP		Z, LBL_dead_scrn_tetris_addy
	JP		LBL_dead_scrn_tetris
LBL_dead_scrn_tetris_addy:
	LD		HL, INT_dead_x
	LD		(HL), 1
	LD		A, (INT_dead_y)
	INC		A
	LD		HL, INT_dead_y
	LD		(HL), A
	CP		16
	RET		Z
	JP		LBL_dead_scrn_tetris
LBL_game_move_down:
	LD		HL, INT_wait_moin_y_2
	LD		(HL), 0
	LD		A, (INT_piece_rely)
	DEC		A
	LD		HL, INT_piece_rely
	LD		(HL), A
	CALL	CAL_update_piece
	RET
LBL_game_move_right:
	LD		A, (INT_piece_relx)
	INC		A
	LD		HL, INT_piece_relx
	LD		(HL), A
	CALL	CAL_update_piece
	RET
LBL_game_move_left:
	LD		A, (INT_piece_relx)
	DEC		A
	LD		HL, INT_piece_relx
	LD		(HL), A
	CALL	CAL_update_piece
	RET
CAL_rot_reset:
	LD		A, 0
	RET
CAL_rot_reset2:
	LD		A, 4
	RET
LBL_game_rotate:
	LD		A, (INT_rot)
	INC		A
	CP		4
	CALL	Z, CAL_rot_reset
	LD		HL, INT_rot
	LD		(HL), A
	CALL	CAL_update_piece
	RET
LBL_game_rotate_minus:
	LD		A, (INT_rot)
	CP		0
	CALL	Z, CAL_rot_reset2
	DEC		A
	LD		HL, INT_rot
	LD		(HL), A
	CALL	CAL_update_piece
	RET
CAL_set_new_fr:
	LD		A, (INT_format)
	LD		E, A
	LD		A, (INT_rot)
	ADD		A, E
	LD		E, A
	RET
CAL_set_anc_fr:
	LD		A, (INT_anc_format)
	LD		E, A
	LD		A, (INT_anc_rot)
	ADD		A, E
	LD		E, A
	RET
CAL_next_format:
	LD		A, (INT_next_format)
	LD		E, A
	RET
CAL_getformat:
	LD		A, (INT_anc_or_new)
	CP		1
	CALL	Z, CAL_set_new_fr
	CP		2
	CALL	Z, CAL_set_anc_fr
	CP		3
	CALL	Z, CAL_next_format
	LD		A, E
	CP		10
	JP		Z, LBL_draw_piece_1_0
	CP		11
	JP		Z, LBL_draw_piece_1_0
	CP		12
	JP		Z, LBL_draw_piece_1_0
	CP		13
	JP		Z, LBL_draw_piece_1_0
	CP		20
	JP		Z, LBL_draw_piece_2_0
	CP		21
	JP		Z, LBL_draw_piece_2_1
	CP		22
	JP		Z, LBL_draw_piece_2_2
	CP		23
	JP		Z, LBL_draw_piece_2_3
	CP		30
	JP		Z, LBL_draw_piece_3_0
	CP		31
	JP		Z, LBL_draw_piece_3_1
	CP		32
	JP		Z, LBL_draw_piece_3_0
	CP		33
	JP		Z, LBL_draw_piece_3_1
	CP		40
	JP		Z, LBL_draw_piece_4_0
	CP		41
	JP		Z, LBL_draw_piece_4_1
	CP		42
	JP		Z, LBL_draw_piece_4_0
	CP		43
	JP		Z, LBL_draw_piece_4_1
	CP		50
	JP		Z, LBL_draw_piece_5_0
	CP		51
	JP		Z, LBL_draw_piece_5_1
	CP		52
	JP		Z, LBL_draw_piece_5_0
	CP		53
	JP		Z, LBL_draw_piece_5_1
	CP		60
	JP		Z, LBL_draw_piece_6_0
	CP		61
	JP		Z, LBL_draw_piece_6_1
	CP		62
	JP		Z, LBL_draw_piece_6_2
	CP		63
	JP		Z, LBL_draw_piece_6_3
	CP		70
	JP		Z, LBL_draw_piece_7_0
	CP		71
	JP		Z, LBL_draw_piece_7_1
	CP		72
	JP		Z, LBL_draw_piece_7_2
	CP		73
	JP		Z, LBL_draw_piece_7_3
	RET
ARR_score:										; Début Score
	DB		48,48,48,48,48,48,48,48,48,0
ARR_score_buf:
	DB		0,0,0,0,0,0,0,0,0,0
CAL_conv_score:
	LD		B, 4
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
    LD		A, (INT_score_10000)
    LD		(HL), A
    LD		B, 5
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
    LD		A, (INT_score_1000)
    LD		(HL), A
    LD		B, 6
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
    LD		A, (INT_score_100)
    LD		(HL), A
    LD		B, 7
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
    LD		A, (INT_score_10)
    LD		(HL), A
    LD		B, 8
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
    LD		A, (INT_score_1)
    LD		(HL), A
    RET
INT_score_10000:
	DB		0
INT_score_1000:
	DB		0
INT_score_100:
	DB		0
INT_score_10:
	DB		0
INT_score_1:
	DB		0
INT_score_to_add:
	DB		12
INT_score_to_add_buf:
	DB		0
INT_score_x:
	DB		0
CAL_add_score:
	CALL	CAL_conv_score
	CALL	CAL_add_score_2
	CALL	CAL_update_score
	RET
CAL_add_score_2:
	LD		HL, INT_score_x
	LD		(HL), 8
	JP		LBL_add_score
LBL_add_score:
    LD		A, (INT_score_x)
    LD		B, A
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
	CALL	CAL_mtx_10
	LD		HL, INT_score_to_add_buf
	LD		(HL), A
	LD		A, (INT_score_x)
    LD		B, A
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score
	CALL	CAL_mtx_10
	LD		E, A
	LD		A, (INT_score_to_add_buf)
	ADD		A, E
	CP		58
	JP		NC, LBL_add_score_next_unite
	LD		(HL), A
	JP		LBL_add_score_addx
LBL_add_score_next_unite:
	SUB		10
	LD		(HL), A
    LD		A, (INT_score_x)
    DEC		A
    LD		B, A
    LD		A, 0
    LD		C, A
    LD		HL, ARR_score_buf
    CALL	CAL_mtx_10
	INC		A
	LD		(HL), A
	JP		LBL_add_score_addx
LBL_add_score_addx:
	LD		A, (INT_score_x)
	DEC		A
	LD		HL, INT_score_x
	LD		(HL), A
	INC		A
	CP		0
	RET		Z
	JP		LBL_add_score
CAL_update_score:
	LD		A, 16
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, ARR_score
	call	_writestring
	RET											; Fin Score
CAL_draw_new_piece:
	LD  	HL, INT_piece_relx
	LD		(HL), 5
	LD  	HL, INT_piece_rely
	LD		(HL), 15
	LD		A, (INT_next_format)
	LD		HL, INT_format
	LD		(HL), A
	LD		HL, INT_wait_moin_y
	LD		(HL), 0
	LD		HL, INT_wait_moin_y_2
	LD		(HL), 0
	CALL	CAL_random
	INC		HL
	ADD		HL, HL
	LD		D, H
	LD		E, L
	ADD		HL, HL
	ADD		HL, HL
	ADD		HL, DE
	LD		D, H
	LD		E, L
	LD		A, H
	ADD		A, L
	LD		HL, INT_next_format
	LD		(HL), A
	LD		HL, INT_rot
	LD		(HL), 0
	LD		HL, INT_anc_or_new
	LD		(HL), 3
	CALL	CAL_draw_next_piece
	CALL	CAL_draw_piece
	RET
CAL_random:
	IN		A, (032h)
	SRL		A
	SRL		A
	SRL		A
	AND		007h
	LD		L, A
	LD		H, 000h
	RET
CAL_draw_piece:
	LD		A, (INT_piece_relx)
	LD  	HL, INT_anc_piece_relx
	LD		(HL), A
	LD		A, (INT_piece_rely)
	LD  	HL, INT_anc_piece_rely
	LD		(HL), A
	LD		A, (INT_format)
	LD		HL, INT_anc_format
	LD		(HL), A
	LD		A, (INT_rot)
	LD		HL, INT_anc_rot
	LD		(HL), A
	CALL	CAL_put_piece_mtx
	RET
CAL_draw_next_piece:							; Début Next Piece
	LD		HL, INT_sp_piece_relx
	LD		(HL), 18
	LD		HL, INT_sp_piece_rely
	LD		(HL), 7
	CALL	CAL_next_piece_mtx
	RET
CAL_next_piece_mtx:
	CALL	CAL_checkup_mtx_1
	LD		HL, INT_ppiece_relx
	LD		(HL), 0
	LD		HL, INT_ppiece_rely
	LD		(HL), 0
	JP		LBL_next_piece_mtx
INT_a_buf:
	DB		0
LBL_next_piece_mtx:
	CALL	CAL_checkup_mtx_2
	LD		HL, INT_a_buf
	LD		(HL), A
	CALL	CAL_ppiece_off
	LD		A, (INT_a_buf)
	CP		1
	CALL	Z, CAL_ppiece_on
	JP		LBL_next_piece_mtx_addx
LBL_next_piece_mtx_addx:
	LD		A, (INT_ppiece_relx)
	INC		A
	LD		HL, INT_ppiece_relx
	LD		(HL), A
	CALL	CAL_checkup_mtx_4
	JP		Z, LBL_next_piece_mtx_addy
	JP		LBL_next_piece_mtx
LBL_next_piece_mtx_addy:
	LD		HL, INT_ppiece_relx
	LD		(HL), 0
	LD		A, (INT_ppiece_rely)
	INC		A
	LD		HL, INT_ppiece_rely
	LD		(HL), A
	CALL	CAL_checkup_mtx_5
	RET		Z
	JP		LBL_next_piece_mtx					; Fin Next Piece
CAL_put_piece_mtx:								; Début Mettre Piece dans MTX
	CALL	CAL_checkup_mtx_1
	JP		LBL_put_piece_mtx
LBL_put_piece_mtx:
	CALL	CAL_checkup_mtx_2
	CP		1
	JP		Z, LBL_put_piece_mtx_add
	JP		LBL_put_piece_mtx_addx
CALL_put_piece_mtx_add_bug:
	INC		A
	LD		(HL), A
	RET
LBL_put_piece_mtx_add:
	CALL	CAL_checkup_mtx_3
	CP		4
	CALL	NZ, CALL_put_piece_mtx_add_bug
	JP		LBL_put_piece_mtx_addx
LBL_put_piece_mtx_addx:
	CALL	CAL_checkup_mtx_4
	JP		Z, LBL_put_piece_mtx_addy
	JP		LBL_put_piece_mtx
LBL_put_piece_mtx_addy:
	CALL	CAL_checkup_mtx_5
	RET		Z
	JP		LBL_put_piece_mtx					; Fin Mettre Piece dans MTX
CAL_check_for_collision:						; Début Check Collision
	CALL	CAL_checkup_mtx_1
	LD		HL, INT_collision_detecte
	LD		(HL), 0
	JP		LBL_check_for_collision
LBL_check_for_collision:
	CALL	CAL_checkup_mtx_2
	CP		1
	JP		Z, LBL_check_for_collision_2
	JP		LBL_check_for_collision_addx
LBL_check_for_collision_2:
	CALL	CAL_checkup_mtx_3
	LD		E, A
	LD		A, E
	CP		3
	JP		Z, CAL_collision_detecte
	JP		LBL_check_for_collision_addx
LBL_check_for_collision_addx:
	CALL	CAL_checkup_mtx_4
	JP		Z, LBL_check_for_collision_addy
	JP		LBL_check_for_collision
LBL_check_for_collision_addy:
	CALL	CAL_checkup_mtx_5
	RET		Z
	JP		LBL_check_for_collision	
CAL_collision_detecte:
	LD		A, (INT_piece_rely)
	LD		E, A
	LD		A, (INT_anc_piece_relx)
	LD  	HL, INT_piece_relx
	LD		(HL), A
	LD		A, (INT_anc_piece_rely)
	LD  	HL, INT_piece_rely
	LD		(HL), A
	LD		A, (INT_anc_format)
	LD		HL, INT_format
	LD		(HL), A
	LD		A, (INT_anc_rot)
	LD		HL, INT_rot
	LD		(HL), A
	LD		HL, INT_collision_detecte
	LD		(HL), 1
	LD		A, (INT_anc_piece_rely)
	CP		E
	JP		NZ, CAL_collision_bas
	RET											; Fin Check Collision
INT_coll_buf2:
	DB		0
CAL_collision_bas:								; Début Collision Bas
	LD		A, (INT_piece_rely)
	CP		15
	JP		Z, LBL_quit
	LD		HL, INT_score_10000
	LD		(HL), 0
	LD		HL, INT_score_1000
	LD		(HL), 0
	LD		HL, INT_score_100
	LD		(HL), 0
	LD		HL, INT_score_10
	LD		(HL), 0
	LD		HL, INT_score_1
	LD		(HL), 8
	CALL	CAL_add_score
	CALL	CAL_checkup_mtx_1
	JP		LBL_collision_bas
LBL_collision_bas:
	CALL	CAL_checkup_mtx_2
	CP		1
	JP		Z, LBL_collision_bas_add
	JP		LBL_collision_bas_addx
LBL_collision_bas_add:
	CALL	CAL_checkup_mtx_3
	LD		(HL), 3
	LD		HL, MTX_skin
	LD		A, (INT_put_piece_mtx_x)
	LD		E, A
	LD		A, (INT_piece_relx)
	ADD		A, E
	LD		B, A
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
 	LD		C, A
	CALL	CAL_mtx_13
	LD		A, (INT_format)
	LD		(HL), A
	JP		LBL_collision_bas_addx
LBL_collision_bas_addx:
	CALL	CAL_checkup_mtx_4
	JP		Z, LBL_collision_bas_addy
	JP		LBL_collision_bas
LBL_collision_bas_addy:
	CALL	CAL_checkup_mtx_5
	LD		HL, INT_coll_buf2
	LD		(HL), A
	CP		4
	CALL	Z, CAL_check_line
	LD		A, (INT_coll_buf2)
	CP		4
	CALL	Z, CAL_draw_new_piece
	LD		A, (INT_coll_buf2)
	RET		Z
	JP		LBL_collision_bas					; Fin Collision Bas
INT_exchange_line:
	DB		0
MTX_exchange_line:
	DB		0, 0, 0, 0
INT_exchange_line_x:
	DB		0
INT_exchange_line_current_y:
	DB		0
INT_exchange_line_many_y:
	DB		0
INT_exchange_line_sum:
	DB		0
INT_exchange_line_last:
	DB		0
INT_exchange_line_VX:
	DB		0
INT_exchange_line_VY:
	DB		0
INT_exchange_line_buf:
	DB		0
INT_exchange_line_0:
	DB		0
LBL_exchange_line:								; Exchange Line
	LD		A, (INT_exchange_line)
	CP		0
	RET		Z
	LD		HL, INT_exchange_line_sum
	LD		(HL), 0
	LD		HL, INT_exchange_line_last
	LD		(HL), 0
	LD		HL, INT_exchange_line_02
	LD		(HL), 0
	JP		LBL_exchange_line_2
LBL_exchange_line_2:
	LD		HL, INT_exchange_line_0
	LD		(HL), 0
	LD		HL, INT_exchange_line_x
	LD		(HL), 0
	LD		HL, MTX_exchange_line
	LD		A, (INT_exchange_line_sum)
	LD		B, A
	LD		C, 0
	CALL	CAL_mtx_4
	LD		HL, INT_exchange_line_current_y
	LD		(HL), A
	LD		A, (INT_exchange_line)
	DEC		A
	LD		E, A
	LD		A, (INT_exchange_line_sum)
	CP		E
	JP		Z, LBL_exchange_line_last
	JP		LBL_schange_line_not_last
LBL_exchange_line_last:
	LD		A, (INT_exchange_line_current_y)
	LD		E, A
	LD		A, 15
	SUB		E
	LD		HL, INT_exchange_line_many_y
	LD		(HL), A
	LD		HL, INT_exchange_line_last
	LD		(HL), 1
	LD		A, (INT_exchange_line_current_y)
	INC		A
	LD		HL, INT_exchange_line_VY
	LD		(HL), A
	LD		A, (INT_exchange_line_sum)
	INC		A
	LD		HL, INT_exchange_line_sum
	LD		(HL), A
	JP		LBL_exchange_line_3
INT_aaad:
	DB		0, 0
LBL_schange_line_not_last:
	LD		A, (INT_exchange_line_sum)
	INC		A
	LD		HL, INT_exchange_line_sum
	LD		(HL), A
	LD		B, A
	LD		C, 0
	LD		HL, MTX_exchange_line
	CALL	CAL_mtx_4
	LD		B, A
	LD		A, (INT_exchange_line_current_y)
	LD		E, A
	LD		A, B
	SUB		E
	DEC		A
	CP		0
	JP		Z, LBL_exchange_line_2
	LD		HL, INT_exchange_line_many_y
	LD		(HL), A
	LD		A, (INT_exchange_line_current_y)
	INC		A
	LD		HL, INT_exchange_line_VY
	LD		(HL), A
	JP		LBL_exchange_line_3
LBL_exchange_line_checkend:
	LD		A, (INT_exchange_line_last)
	CP		1
	RET		Z
	JP		LBL_exchange_line_2
LBL_exchange_line_3:
	LD		HL, INT_exchange_line_0
	LD		(HL), 0
	LD		HL, INT_exchange_line_VX
	LD		(HL), 1
	JP		LBL_exchange_line_4
LBL_exchange_line_3_addx:
	LD		HL, INT_exchange_line_02
	LD		(HL), 0
	LD		A, (INT_exchange_line_many_y)
	LD		E, A
	LD		A, (INT_exchange_line_x)
	INC		A
	LD		HL, INT_exchange_line_x
	LD		(HL), A
	CP		E
	JP		Z, LBL_exchange_line_checkend
	LD		A, (INT_exchange_line_VY)
	INC		A
	LD		HL, INT_exchange_line_VY
	LD		(HL), A
	CP		16
	JP		NC, LBL_exchange_line_checkend
	JP		LBL_exchange_line_3
LBL_exchange_line_4:
	LD		HL, MTX_collision
	LD		A, (INT_exchange_line_VX)
	LD		B, A
	LD		A, (INT_exchange_line_VY)
 	LD		C, A
	CALL	CAL_mtx_13
	LD		(HL), 0
	LD		HL, INT_exchange_line_buf
	LD		(HL), A
	LD		HL, MTX_collision
	LD		A, (INT_exchange_line_VX)
	LD		B, A
	LD		A, (INT_exchange_line_sum)
	LD		E, A
	LD		A, (INT_exchange_line_VY)
	SUB		E
 	LD		C, A
	CALL	CAL_mtx_13
	LD		A, (INT_exchange_line_buf)
	LD		(HL), A
	LD		HL, MTX_skin
	LD		A, (INT_exchange_line_VX)
	LD		B, A
	LD		A, (INT_exchange_line_VY)
 	LD		C, A
	CALL	CAL_mtx_13
	LD		(HL), 0
	CALL	CAL_exc_sp
	LD		A, (INT_piece_skin)
	CP		0
	CALL	Z, CAL_exc_sp2
	LD		A, (INT_exchange_line_VX)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_exchange_line_VY)
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	CALL	CAL_ppiece_off
	LD		HL, MTX_skin
	LD		A, (INT_exchange_line_VX)
	LD		B, A
	LD		A, (INT_exchange_line_sum)
	LD		E, A
	LD		A, (INT_exchange_line_VY)
	SUB		E
 	LD		C, A
	CALL	CAL_mtx_13
	LD		A, (INT_piece_skin)
	ADD		A, A
	LD		E, A
	ADD		A, A
	ADD		A, A
	ADD		A, E
	LD		(HL), A
	LD		A, (INT_exchange_line_VX)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_exchange_line_sum)
	LD		E, A
	LD		A, (INT_exchange_line_VY)
	SUB		E
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	LD		A, (INT_piece_skin)
	CP		0
	CALL	Z, CAL_ppiece_off
	LD		A, (INT_piece_skin)
	CP		0
	CALL	NZ, CAL_ppiece_on
	JP		LBL_exchange_line_4_addx
CAL_exc_sp2:
	LD		A, (INT_exchange_line_last)
	CP		1
	JP		Z, LBL_exc_sp3
	RET
LBL_exc_sp3:
	LD		A, (INT_exchange_line_0)
	INC		A
	LD		HL, INT_exchange_line_0
	LD		(HL), A
	RET
INT_exchange_line_02:
	DB		0
CAL_exc_sp:
	CP		0
	CALL	Z, CAL_exc_sp_0
	CP		10
	CALL	Z, CAL_exc_sp_1
	CP		20
	CALL	Z, CAL_exc_sp_2
	CP		30
	CALL	Z, CAL_exc_sp_3
	CP		40
	CALL	Z, CAL_exc_sp_4
	CP		50
	CALL	Z, CAL_exc_sp_5
	CP		60
	CALL	Z, CAL_exc_sp_6
	CP		70
	CALL	Z, CAL_exc_sp_7
	RET
CAL_exc_sp_0:
	LD		HL, INT_piece_skin
	LD		(HL), 0
	RET
CAL_exc_sp_1:
	LD		HL, INT_piece_skin
	LD		(HL), 1
	RET
CAL_exc_sp_2:
	LD		HL, INT_piece_skin
	LD		(HL), 2
	RET
CAL_exc_sp_3:
	LD		HL, INT_piece_skin
	LD		(HL), 3
	RET
CAL_exc_sp_4:
	LD		HL, INT_piece_skin
	LD		(HL), 4
	RET
CAL_exc_sp_5:
	LD		HL, INT_piece_skin
	LD		(HL), 5
	RET
CAL_exc_sp_6:
	LD		HL, INT_piece_skin
	LD		(HL), 6
	RET
CAL_exc_sp_7:
	LD		HL, INT_piece_skin
	LD		(HL), 7
	RET
LBL_exchange_line_4_addx:
	LD		A, (INT_exchange_line_0)
	CP		11
	RET		Z
	LD		A, (INT_exchange_line_VX)
	INC		A
	LD		HL, INT_exchange_line_VX
	LD		(HL), A
	CP		12
	JP		Z, LBL_exchange_line_3_addx
	JP		LBL_exchange_line_4
CAL_check_line:									; CHECK LINE !
	LD		HL, INT_exchange_line
	LD		(HL), 0
	CALL	CAL_checkup_mtx_1
	JP		LBL_check_line
LBL_check_line:
	LD		HL, INT_check_line_x
	LD		(HL), 0
	LD		HL, INT_check_line_cumsum
	LD		(HL), 0
	CALL	CAL_checkup_mtx_2
	CP		1
	JP		Z, LBL_check_line_2
	JP		LBL_check_line_addx
LBL_check_line_addx:
	CALL	CAL_checkup_mtx_4
	JP		Z, LBL_check_line_addy
	JP		LBL_check_line
LBL_check_line_addy:
	CALL	CAL_checkup_mtx_5
	JP		Z, LBL_exchange_line
	JP		LBL_check_line
LBL_check_line_2:
	LD		HL, MTX_collision
	LD		A, (INT_check_line_x)
	LD		B, A
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
 	LD		C, A
	CALL	CAL_mtx_13
	LD		E, A
	LD		A, (INT_check_line_cumsum)
	ADD		A, E
	LD		HL, INT_check_line_cumsum
	LD		(HL), A
	JP		LBL_check_line_2_addx
LBL_check_line_2_addx:
	LD		A, (INT_check_line_x)
	INC		A
	LD		HL, INT_check_line_x
	LD		(HL), A
	CP		13
	JP		Z, LBL_check_line_2_end
	JP		LBL_check_line_2
LBL_check_line_2_end:
	LD		A, (INT_check_line_cumsum)
	CP		39
	JP		Z, LBL_erase_line
	JP		LBL_check_line_addy
CAL_add_ligne:
	LD		HL, INT_score_10000
	LD		(HL), 0
	LD		HL, INT_score_1000
	LD		(HL), 0
	LD		A, (INT_exchange_line)
	INC		A
	LD		HL, INT_score_100
	LD		(HL), A
	LD		HL, INT_score_10
	LD		(HL), 0
	LD		HL, INT_score_1
	LD		(HL), 0
	CALL	CAL_add_score
	LD		A, (INT_nb_ligne)
	INC		A
	LD		HL, INT_nb_ligne
	LD		(HL), A
	CP		10
	JP		Z, LBL_next_level
	RET
LBL_next_level:
	LD		HL, INT_nb_ligne
	LD		(HL), 0
	LD		A, (INT_speed_1)
	DEC		A
	LD		HL, INT_speed_1
	LD		(HL), A
	LD		A, (INT_lvl)
	INC		A
	LD		HL, INT_lvl
	LD		(HL), A
	CP		10
	JP		Z, LBL_next_level10
	LD		A, (INT_lvl2)
	ADD		A, 48
	LD		HL, INT_aaap
	LD		(HL), A
	LD		A, 26
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, INT_aaap
	call	_writestring
	LD		A, (INT_lvl)
	ADD		A, 48
	LD		HL, INT_aaap
	LD		(HL), A
	LD		A, 27
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, INT_aaap
	call	_writestring
	RET
LBL_next_level10:
	LD		HL, INT_lvl
	LD		(HL), 0
	LD		A, (INT_lvl2)
	INC		A
	LD		HL, INT_lvl2
	LD		(HL), A
	LD		A, (INT_lvl2)
	ADD		A, 48
	LD		HL, INT_aaap
	LD		(HL), A
	LD		A, 26
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, INT_aaap
	call	_writestring
	LD		A, (INT_lvl)
	ADD		A, 48
	LD		HL, INT_aaap
	LD		(HL), A
	LD		A, 27
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, INT_aaap
	call	_writestring
	RET
INT_lvl2:
	DB		0
INT_aaap:
	DB		0,0
LBL_erase_line:
	CALL	CAL_add_ligne
	LD		HL, MTX_exchange_line
	LD		A, (INT_exchange_line)
	LD		B, A
	LD		C, 0
	CALL	CAL_mtx_4
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
	LD		(HL), A
	LD		A, (INT_exchange_line)
	INC		A
	LD		HL, INT_exchange_line
	LD		(HL), A
	LD		HL, INT_check_line_x
	LD		(HL), 1
	JP		LBL_erase_line_2
LBL_erase_line_2:
	LD		HL, MTX_collision
	LD		A, (INT_check_line_x)
	LD		B, A
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
 	LD		C, A
	CALL	CAL_mtx_13
	LD		(HL), 0
	CALL	CAL_update_reset_relxy
	LD		A, (INT_check_line_x)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	CALL	CAL_ppiece_off
	JP		LBL_erase_line_2_addx
LBL_erase_line_2_addx:
	LD		A, (INT_check_line_x)
	INC		A
	LD		HL, INT_check_line_x
	LD		(HL), A
	CP		12
	JP		Z, LBL_check_line_addy
	JP		LBL_erase_line_2
INT_check_line_x:
	DB		0
INT_check_line_cumsum:
	DB		0
CAL_checkup_mtx_1:
	LD		HL, INT_put_piece_mtx_x
	LD		(HL), 0
	LD		HL, INT_put_piece_mtx_y
	LD		(HL), 0
	RET
CAL_checkup_mtx_2:
	CALL	CAL_getformat
	LD		A, (INT_put_piece_mtx_x)
	LD		B, A
	LD		A, (INT_put_piece_mtx_y)
 	LD		C, A
	CALL	CAL_mtx_4
	RET
CAL_checkup_mtx_3:
	LD		HL, MTX_collision
	LD		A, (INT_put_piece_mtx_x)
	LD		E, A
	LD		A, (INT_piece_relx)
	ADD		A, E
	LD		B, A
	LD		A, (INT_put_piece_mtx_y)
	LD		E, A
	LD		A, (INT_piece_rely)
	ADD		A, E
 	LD		C, A
	CALL	CAL_mtx_13
	RET
CAL_checkup_mtx_4:
	LD		A, (INT_put_piece_mtx_x)
	INC		A
	LD		HL, INT_put_piece_mtx_x
	LD		(HL), A
	CP		4
	RET
CAL_checkup_mtx_5:
	LD		HL, INT_put_piece_mtx_x
	LD		(HL), 0
	LD		A, (INT_put_piece_mtx_y)
	INC		A
	LD		HL, INT_put_piece_mtx_y
	LD		(HL), A
	CP		4
	RET
CAL_update_piece:								; Début Update Piece
	LD		HL, INT_anc_or_new
	LD		(HL), 1
	CALL	CAL_check_for_collision
	LD		A, (INT_collision_detecte)
	CP		1
	RET		Z
	CALL	CAL_put_piece_mtx
	CALL	CAL_update_reset_relxy
	LD		A, (INT_piece_relx)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_piece_rely)
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	CALL	CAL_update_new_piece
	CALL	CAL_update_reset_relxy
	LD		A, (INT_anc_piece_relx)
	LD		HL, INT_sp_piece_relx
	LD		(HL), A
	LD		A, (INT_anc_piece_rely)
	LD		HL, INT_sp_piece_rely
	LD		(HL), A
	LD		HL, INT_anc_or_new
	LD		(HL), 2
	CALL	CAL_update_anc_piece
	LD		A, (INT_piece_relx)
	LD  	HL, INT_anc_piece_relx
	LD		(HL), A
	LD		A, (INT_piece_rely)
	LD  	HL, INT_anc_piece_rely
	LD		(HL), A
	LD		A, (INT_format)
	LD		HL, INT_anc_format
	LD		(HL), A
	LD		A, (INT_rot)
	LD		HL, INT_anc_rot
	LD		(HL), A
	RET
CAL_update_new_piece:
	JP		LBL_update_new_piece	
LBL_update_new_piece:
	CALL	CAL_update_mtx_format
	CP		1
	JP		Z, LBL_update_new_piece_2
	JP		LBL_update_new_piece_addx
LBL_update_new_piece_2:
	CALL	CAL_update_mtx_collision
	CP		1
	CALL	Z, CAL_ppiece_on
	JP		LBL_update_new_piece_addx
LBL_update_new_piece_addx:
	LD		A, (INT_ppiece_relx)
	INC		A
	LD		HL, INT_ppiece_relx
	LD		(HL), A
	CP		4
	JP		Z, LBL_update_new_piece_addy
	JP		LBL_update_new_piece
LBL_update_new_piece_addy:
	LD		HL, INT_ppiece_relx
	LD		(HL), 0
	LD		A, (INT_ppiece_rely)
	INC		A
	LD		HL, INT_ppiece_rely
	LD		(HL), A
	CP		4
	RET		Z
	JP		LBL_update_new_piece
CAL_update_anc_piece:
	JP		LBL_update_anc_piece
LBL_update_anc_piece:
	CALL	CAL_update_mtx_format
	CP		1
	JP		Z, LBL_update_anc_piece_2
	JP		LBL_update_anc_piece_addx
LBL_update_anc_piece_2:
	CALL	CAL_update_mtx_collision
	CP		1
	CALL	Z, CAL_anc_ppiece_off_1
	CP		2
	CALL	Z, CAL_anc_ppiece_off_2
	JP		LBL_update_anc_piece_addx
CAL_anc_ppiece_off_1:
	LD		(HL), 0
	CALL	CAL_ppiece_off
	RET
CAL_anc_ppiece_off_2:
	LD		(HL), 1
	RET
LBL_update_anc_piece_addx:
	LD		A, (INT_ppiece_relx)
	INC		A
	LD		HL, INT_ppiece_relx
	LD		(HL), A
	CP		4
	JP		Z, LBL_update_anc_piece_addy
	JP		LBL_update_anc_piece
LBL_update_anc_piece_addy:
	LD		HL, INT_ppiece_relx
	LD		(HL), 0
	LD		A, (INT_ppiece_rely)
	INC		A
	LD		HL, INT_ppiece_rely
	LD		(HL), A
	CP		4
	RET		Z
	JP		LBL_update_anc_piece
CAL_update_mtx_collision:
	LD		HL, MTX_collision
	LD		A, (INT_ppiece_relx)
	LD		E, A
	LD		A, (INT_sp_piece_relx)
	ADD		A, E
	LD		B, A
	LD		A, (INT_ppiece_rely)
	LD		E, A
	LD		A, (INT_sp_piece_rely)
	ADD		A, E
 	LD		C, A
 	CALL	CAL_mtx_13
 	RET
CAL_update_mtx_format:
	CALL	CAL_getformat
	LD		A, (INT_ppiece_relx)
	LD		B, A
	LD		A, (INT_ppiece_rely)
 	LD		C, A
	CALL	CAL_mtx_4
	RET
CAL_update_reset_relxy:
	LD  	HL, INT_ppiece_relx
	LD		(HL), 0
	LD  	HL, INT_ppiece_rely
	LD		(HL), 0
	RET											; Fin Update Piece
CAL_getskin:
	LD		A, (INT_ppiece_on)
	CP		0
	JP		Z, LBL_ppiece_skin_0
	LD		A, (INT_piece_skin)
	CP		1
	JP		Z, LBL_ppiece_skin_1
	CP		2
	JP		Z, LBL_ppiece_skin_2
	CP		3
	JP		Z, LBL_ppiece_skin_3
	CP		4
	JP		Z, LBL_ppiece_skin_4
	CP		5
	JP		Z, LBL_ppiece_skin_5
	CP		6
	JP		Z, LBL_ppiece_skin_6
	CP		7
	JP		Z, LBL_ppiece_skin_7
	RET
CAL_ppiece_on:									; Début Affiche une ppiece
	LD		HL, INT_ppiece_on
	LD		(HL), 1
	LD		HL, INT_ppiece_x
	LD		(HL), 0
	LD		HL, INT_ppiece_y
 	LD		(HL), 0
	JP		LBL_ppiece_on
CAL_ppiece_off:
	LD		HL, INT_ppiece_on
	LD		(HL), 0
	LD		HL, INT_ppiece_x
	LD		(HL), 0
	LD		HL, INT_ppiece_y
 	LD		(HL), 0
	JP		LBL_ppiece_on
LBL_ppiece_on:
	CALL	CAL_getskin
	LD		A, (INT_ppiece_x)
	LD		B, A
	LD		A, (INT_ppiece_y)
 	LD		C, A
	CALL	CAL_mtx_4
	CP		1
	CALL	Z, CAL_ppiece_on_2
	JP		LBL_ppiece_addx
CAL_ppiece_on_2:
	CALL	CAL_ppiece_get_absxy
	LD		A, (INT_ppiece_on)
	LD		D, A
;			B_CALL(_IPoint)
	RET
LBL_ppiece_addx:
	LD		A, (INT_ppiece_x)
	INC		A
	LD		HL, INT_ppiece_x
	LD		(HL), A
	CP		4
	JP		Z, LBL_ppiece_addy
	JP		LBL_ppiece_on
LBL_ppiece_addy:
	LD		HL, INT_ppiece_x
	LD		(HL), 0
	LD		A, (INT_ppiece_y)
	INC		A
	LD		HL, INT_ppiece_y
	LD		(HL), A
	CP		4
	RET		Z
	JP		LBL_ppiece_on
CAL_ppiece_get_absxy:
	LD		A, (INT_ppiece_relx)
	LD		E, A
	LD		A, (INT_sp_piece_relx)
	ADD		A, E
	ADD		A, A
	ADD		A, A
	ADD		A, 6
	LD		E, A
	LD		A, (INT_ppiece_x)
	ADD		A, E
	LD		B, A
	LD		A, (INT_ppiece_rely)
	LD		E, A
	LD		A, (INT_sp_piece_rely)
	ADD		A, E
	ADD		A, A
	ADD		A, A
	LD		E, A
	LD		A, (INT_ppiece_y)
	ADD		A, E
	LD		C, A
    RET											; Fin Affiche une ppiece
CAL_draw_arena:
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 1
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 2
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 3
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 4
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 5
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 6
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 8
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 9
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 10
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 11
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 12
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 13
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 14
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 15
	LD		(_cur_y), A
	LD		HL, str_arena1
	call	_writestring
	LD		A, 1
	LD		(_cur_x), A
	LD		A, 16
	LD		(_cur_y), A
	LD		HL, str_arena2
	call	_writestring
    LD		A, 16
	LD		(_cur_x), A
	LD		A, 1
	LD		(_cur_y), A
	LD		HL, MSG_tetris_title
	call	_writestring
	LD		A, 28
	LD		(_cur_x), A
	LD		A, 1
	LD		(_cur_y), A
	LD		HL, MSG_tetris_ver
	call	_writestring
	LD		A, 16
	LD		(_cur_x), A
	LD		A, 2
	LD		(_cur_y), A
	LD		HL, MSG_next
	call	_writestring
	LD		A, 16
	LD		(_cur_x), A
	LD		A, 5
	LD		(_cur_y), A
	LD		HL, MSG_score
	call	_writestring
	LD		A, 18
	LD		(_cur_x), A
	LD		A, 6
	LD		(_cur_y), A
	LD		HL, MSG_score_pts
	call	_writestring
	LD		A, 16
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, MSG_level
	call	_writestring
	LD		A, 26
	LD		(_cur_x), A
	LD		A, 7
	LD		(_cur_y), A
	LD		HL, MSG_level_pts
	call	_writestring
	LD		A, 16
	LD		(_cur_x), A
	LD		A, 16
	LD		(_cur_y), A
	LD		HL, MSG_ATEMIS
	call	_writestring
	RET
str_arena1:
	DB "|          |",0
str_arena2:
	DB "|__________|",0
MSG_tetris_title:
    DB "TETRIS  jd",0
MSG_tetris_ver:
    DB "v1.1",0
MSG_next:
    DB "Next",0
MSG_level:
    DB "Level :",0
MSG_score:
    DB "Score :",0
MSG_level_pts:
    DB "00",0
MSG_score_pts:
    DB "000000000",0
MSG_ATEMIS:
    DB "ATEMIS",0
LBL_draw_piece_1_0:
	LD		HL, INT_piece_skin
	LD		(HL), 1
	LD		HL, MTX_piece_format_1_0
	RET
LBL_draw_piece_2_0:
	LD		HL, INT_piece_skin
	LD		(HL), 2
	LD		HL, MTX_piece_format_2_0
	RET
LBL_draw_piece_2_1:
	LD		HL, INT_piece_skin
	LD		(HL), 2
	LD		HL, MTX_piece_format_2_1
	RET
LBL_draw_piece_2_2:
	LD		HL, INT_piece_skin
	LD		(HL), 2
	LD		HL, MTX_piece_format_2_2
	RET
LBL_draw_piece_2_3:
	LD		HL, INT_piece_skin
	LD		(HL), 2
	LD		HL, MTX_piece_format_2_3
	RET
LBL_draw_piece_3_0:
	LD		HL, INT_piece_skin
	LD		(HL), 3
	LD		HL, MTX_piece_format_3_0
	RET
LBL_draw_piece_3_1:
	LD		HL, INT_piece_skin
	LD		(HL), 3
	LD		HL, MTX_piece_format_3_1
	RET
LBL_draw_piece_4_0:
	LD		HL, INT_piece_skin
	LD		(HL), 4
	LD		HL, MTX_piece_format_4_0
	RET
LBL_draw_piece_4_1:
	LD		HL, INT_piece_skin
	LD		(HL), 4
	LD		HL, MTX_piece_format_4_1
	RET
LBL_draw_piece_5_0:
	LD		HL, INT_piece_skin
	LD		(HL), 5
	LD		HL, MTX_piece_format_5_0
	RET
LBL_draw_piece_5_1:
	LD		HL, INT_piece_skin
	LD		(HL), 5
	LD		HL, MTX_piece_format_5_1
	RET
LBL_draw_piece_6_0:
	LD		HL, INT_piece_skin
	LD		(HL), 6
	LD		HL, MTX_piece_format_6_0
	RET
LBL_draw_piece_6_1:
	LD		HL, INT_piece_skin
	LD		(HL), 6
	LD		HL, MTX_piece_format_6_1
	RET
LBL_draw_piece_6_2:
	LD		HL, INT_piece_skin
	LD		(HL), 6
	LD		HL, MTX_piece_format_6_2
	RET
LBL_draw_piece_6_3:
	LD		HL, INT_piece_skin
	LD		(HL), 6
	LD		HL, MTX_piece_format_6_3
	RET
LBL_draw_piece_7_0:
	LD		HL, INT_piece_skin
	LD		(HL), 7
	LD		HL, MTX_piece_format_7_0
	RET
LBL_draw_piece_7_1:
	LD		HL, INT_piece_skin
	LD		(HL), 7
	LD		HL, MTX_piece_format_7_1
	RET
LBL_draw_piece_7_2:
	LD		HL, INT_piece_skin
	LD		(HL), 7
	LD		HL, MTX_piece_format_7_2
	RET
LBL_draw_piece_7_3:
	LD		HL, INT_piece_skin
	LD		(HL), 7
	LD		HL, MTX_piece_format_7_3
	RET
LBL_ppiece_skin_0:
	LD		HL, MTX_ppiece_skin_0
	RET
LBL_ppiece_skin_1:
	LD		HL, MTX_ppiece_skin_1
	RET
LBL_ppiece_skin_2:
	LD		HL, MTX_ppiece_skin_2
	RET
LBL_ppiece_skin_3:
	LD		HL, MTX_ppiece_skin_3
	RET
LBL_ppiece_skin_4:
	LD		HL, MTX_ppiece_skin_4
	RET
LBL_ppiece_skin_5:
	LD		HL, MTX_ppiece_skin_5
	RET
LBL_ppiece_skin_6:
	LD		HL, MTX_ppiece_skin_6
	RET
LBL_ppiece_skin_7:
	LD		HL, MTX_ppiece_skin_7
	RET
CAL_mtx_4:
	LD		A, C
 	ADD		A, A
	ADD		A, A
	ADD		A, B
	LD		D, 0
	LD		E, A
	ADD		HL, DE
	LD		A, (HL)
	RET
CAL_mtx_10:
	LD		A, C
 	ADD		A, A
 	LD		E, A
	ADD		A, A
	ADD		A, A
	ADD		A, E
	ADD		A, B
	LD		D, 0
	LD		E, A
	ADD		HL, DE
	LD		A, (HL)
	RET
CAL_mtx_13:
	LD		A, C
	LD		E, A
	ADD		A, A
    ADD		A, E
    ADD		A, A
    ADD		A, A
    ADD		A, E
	ADD		A, B
	LD		D, 0
	LD		E, A
	ADD		HL, DE
	LD		A, (HL)
	RET
CAL_mtx_18:
	LD		A, C
	ADD		A, A
	LD		E, A
	ADD		A, A
	ADD		A, A
	ADD		A, A
	ADD		A, E
	ADD		A, B
	LD		D, 0
	LD		E, A
	ADD		HL, DE
	LD		A, (HL)
	RET
INT_nb_ligne:
	DB		0
INT_lvl:
	DB		0, 0
INT_next_format:
	DB		0
INT_anc_or_new:
	DB		0
INT_collision_detecte:
	DB		0
INT_quit:
	DB		0
INT_piece_skin:
	DB		0
INT_format:
	DB		0
INT_rot:
	DB		0
INT_anc_format:
	DB		0
INT_anc_rot:
	DB		0
INT_speed_1:
	DB		0
INT_wait_moin_y:
	DB		0
INT_wait_moin_y_2:
	DB		0
INT_sp_piece_relx:
	DB		0
INT_sp_piece_rely:
	DB		0
INT_put_piece_mtx_x:
	DB		0
INT_put_piece_mtx_y:
	DB		0
INT_ppiece_on:
	DB		0
INT_ppiece_skin:
	DB		0
INT_piece_format_rot:
	DB		00
INT_anc_piece_format_rot:
	DB		00
INT_piece_rot:
	DB		0
INT_piece_format:
	DB		0
INT_ppiece_x:
	DB		0
INT_ppiece_y:
	DB		0
INT_ppiece_relx:
	DB		0
INT_ppiece_rely:
	DB		0
INT_piece_x:
	DB		0
INT_piece_y:
	DB		0
INT_anc_piece_relx:
	DB		0
INT_anc_piece_rely:
	DB		0
INT_piece_relx:
	DB		0
INT_piece_rely:
	DB		0
STR_bienvenue_1:
	DB		"Tetris By Jean-Denis Boivin", 0
STR_bienvenue_2:
	DB		"Programmed in ASM !", 0
STR_bienvenue_3:
	DB		"Oh My Precious Lord !", 0
STR_bienvenue_5:
	DB		"http://team-atemis.com/", 0
STR_copyright_1:
	DB		"(c) 2006 AtemiS", 0
STR_choose_level:
	DB		"Level", 0
STR_highscore:
	DB		"Highscore", 0
MTX_ppiece_skin_0:
	DB		1,1,1,1
	DB		1,1,1,1
	DB		1,1,1,1
	DB		1,1,1,1
MTX_ppiece_skin_1:
	DB		1,1,1,1
	DB		1,0,0,1
	DB		1,0,0,1
	DB		1,1,1,1
MTX_ppiece_skin_2:
	DB		0,0,0,0
	DB		0,1,1,0
	DB		0,1,1,0
	DB		0,0,0,0
MTX_ppiece_skin_3:
	DB		1,0,1,0
	DB		0,0,0,1
	DB		1,0,0,0
	DB		0,1,0,1
MTX_ppiece_skin_4:
	DB		1,0,1,0
	DB		0,1,0,1
	DB		1,0,1,0
	DB		0,1,0,1
MTX_ppiece_skin_5:
	DB		1,0,1,1
	DB		0,0,1,1
	DB		1,1,0,0
	DB		1,1,0,1
MTX_ppiece_skin_6:
	DB		1,1,1,1
	DB		1,1,1,1
	DB		1,1,1,1
	DB		1,1,1,1
MTX_ppiece_skin_7:
	DB		1,0,0,1
	DB		0,1,1,0
	DB		0,1,1,0
	DB		1,0,0,1
MTX_piece_format_1_0:
	DB		0,0,0,0
	DB		0,1,1,0
	DB		0,1,1,0
	DB		0,0,0,0
MTX_piece_format_2_0:
	DB		0,0,0,0
	DB		0,1,0,0
	DB		1,1,1,0
	DB		0,0,0,0
MTX_piece_format_2_1:
	DB		0,0,0,0
	DB		0,1,0,0
	DB		1,1,0,0
	DB		0,1,0,0
MTX_piece_format_2_2:
	DB		0,0,0,0
	DB		0,0,0,0
	DB		1,1,1,0
	DB		0,1,0,0
MTX_piece_format_2_3:
	DB		0,0,0,0
	DB		0,1,0,0
	DB		0,1,1,0
	DB		0,1,0,0
MTX_piece_format_3_0:
	DB		0,0,0,0
	DB		0,0,0,0
	DB		1,1,1,1
	DB		0,0,0,0
MTX_piece_format_3_1:
	DB		0,1,0,0
	DB		0,1,0,0
	DB		0,1,0,0
	DB		0,1,0,0
MTX_piece_format_4_0:
	DB		0,0,0,0
	DB		1,1,0,0
	DB		0,1,1,0
	DB		0,0,0,0
MTX_piece_format_4_1:
	DB		0,0,0,0
	DB		0,1,0,0
	DB		1,1,0,0
	DB		1,0,0,0
MTX_piece_format_5_0:
	DB		0,0,0,0
	DB		0,1,1,0
	DB		1,1,0,0
	DB		0,0,0,0
MTX_piece_format_5_1:
	DB		0,0,0,0
	DB		1,0,0,0
	DB		1,1,0,0
	DB		0,1,0,0
MTX_piece_format_6_0:
	DB		0,0,0,0
	DB		0,0,1,0
	DB		1,1,1,0
	DB		0,0,0,0
MTX_piece_format_6_1:
	DB		1,1,0,0
	DB		0,1,0,0
	DB		0,1,0,0
	DB		0,0,0,0
MTX_piece_format_6_2:
	DB		0,0,0,0
	DB		1,1,1,0
	DB		1,0,0,0
	DB		0,0,0,0
MTX_piece_format_6_3:
	DB		0,1,0,0
	DB		0,1,0,0
	DB		0,1,1,0
	DB		0,0,0,0
MTX_piece_format_7_0:
	DB		0,0,0,0
	DB		1,0,0,0
	DB		1,1,1,0
	DB		0,0,0,0
MTX_piece_format_7_1:
	DB		0,1,0,0
	DB		0,1,0,0
	DB		1,1,0,0
	DB		0,0,0,0
MTX_piece_format_7_2:
	DB		0,0,0,0
	DB		1,1,1,0
	DB		0,0,1,0
	DB		0,0,0,0
MTX_piece_format_7_3:
	DB		0,1,1,0
	DB		0,1,0,0
	DB		0,1,0,0
	DB		0,0,0,0
MTX_collision:
	DB		3,3,3,3,3,3,3,3,3,3,3,3,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,0,0,0,0,0,0,0,0,0,0,0,3
	DB		3,4,4,4,4,4,4,4,4,4,4,4,3
	DB		3,4,4,4,4,4,4,4,4,4,4,4,3
	DB		3,4,4,4,4,4,4,4,4,4,4,4,3
	DB		3,4,4,4,4,4,4,4,4,4,4,4,3
MTX_skin:
	DB		9,9,9,9,9,9,9,9,9,9,9,9,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,0,0,0,0,0,0,0,0,0,0,0,9
	DB		9,8,8,8,8,8,8,8,8,8,8,8,9
	DB		9,8,8,8,8,8,8,8,8,8,8,8,9
	DB		9,8,8,8,8,8,8,8,8,8,8,8,9
	DB		9,8,8,8,8,8,8,8,8,8,8,8,9

MTX_screen:
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0
	DB		0,0,0,0,0,0,0,0,0,0

; BLOCKY MODE 10x20 PIXELS
UPDATE_BLOCKY:
	PUSH	IX
	LD		IX, 04084h				; Line 2 Column 2
	LD		HL, MTX_screen
	LD		B, 10
UPDATE_NEXTROW:
	PUSH	IX
	PUSH	BC
	LD		B, 5
UPDATE_ROW:
	LD		DE, 10
	LD		A, (HL)
	SLA		A
	INC		HL
	LD		C, (HL)
	OR		C
	SLA		A
	DEC		HL
	ADD		HL, DE
	LD		C, (HL)
	OR		C
	SLA		A
	INC		HL
	LD		C, (HL)
	OR		C
	PUSH	HL
	LD		HL, BLOCKY
	LD		D, 0
	LD		E, A
	LD		DE, 10
	LD		(IX), A
	INC		IX
	INC		IX
	INC		HL
	SUB		HL, DE
	DJNZ	UPDATE_ROW
	ADD		HL, DE
	POP		BC
	POP		IX
	LD		DE, 64
	ADD		IX, DE
	DJNZ	UPDATE_NEXTROW
	RET

BLOCKY:
	DB		020h, 0b8h, 0b4h, 0bch
	DB		0b2h, 0bah, 0b6h, 0beh
	DB		0b1h, 0b9h, 0b5h, 0bdh
	DB		0b3h, 0bbh, 0b7h, 0bfh
	

;; End of Main Application

;; Fill to end of file
;	org 03fffh
	seek 030ffh
	defb 000h
