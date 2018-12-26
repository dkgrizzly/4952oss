include "lib/header.asm"
include "lib/strap.asm"

	org 02071h
	seek 00871h
_splash_screen_data:
	defb 0ffh

	defb 002h, 00ch, 083h
	defb "HP 4952A", 000h
	defb 003h, 006h, 083h
	defb "Open Source Software", 000h

	defb 006h, 006h, 083h
	defb "Blocky Graphics Demo", 000h

	defb 00ch, 008h, 083h
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, 083h
	defb "on hackaday.io", 000h

	defb 000h			;; End of Screen Data

_splash_menu_data:
	defb "Re-!BERT!Remote!Mass !Run !Self~"
	defb "set!Menu!&Print!Store!Demo!Test|"

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
	defw _p_main_menu_page_two	;; Next Page

_p_main_menu_page_two:
	defw _splash_menu_data		;; Second Page Menu Data
_p_mm_reset:
	defw 0bb1ah			;; Entry Point for Re-Set
_p_mm_bert:
	defw 0b22ch			;; Entry Point for BERT Menu
_p_mm_remote:
	defw 0d963h			;; Entry Point for Remote & Print
_p_mm_masstorage:
	defw 00f0ch			;; Entry Point for Mass Storage
_p_mm_launch_app:
	defw _launch_app		;; Entry Point for Application
_p_mm_selftest:
	defw 0136fh			;; Ordinal 12ah Self Test
_p_mm_next2:
	defw _p_main_menu_page_one	;; Next Page

_launch_app:
	ld a, 006h
	call 00e60h			; Page in 6
	ld hl,0aa00h			; Copy application to Work RAM
	ld de,_code_start		;
	ld bc,_code_end-_code_start	;
	ldir				;
	jp _app_main			; Run the application

_splash_end:
;; End of menu section

;; Main Application
	org 2200h
	seek 0a00h
_code_start:

_cnt_frames:
	defw 00800h

_str_finished:
	defb "Goodbye.", 000h

_str_exit:
	defb "Are you sure you wish to exit?", 000h

; BLOCKY MODE 64x32 PIXELS
_update_blocky:
	push ix
	ld ix, 04000h			; Line 1 Column 1
	ld hl, gfx_screen
	ld b, 16			; Number of blocks vertically
_update_nextrow:
	push bc
	;push ix			; If width != screen uncomment this
	ld b, 32			; Number of blocks horizontally
_update_row:
	ld a, (hl)			; Top-Left Pixel from cluster
	sla a
	inc hl

	ld c, (hl)			; Top-Right Pixel from cluster
	or c
	sla a

	inc hl				; Save next cluster address
	push hl

	ld de, 62
	add hl, de
	ld c, (hl)			; Bottom-Left Pixel from cluster
	or c
	sla a
	inc hl

	ld c, (hl)			; Bottom-Right Pixel from cluster
	or c

	sla a				; Shift one more for the attribute

	; A now contains 000LRlr0 which we use to index into the blocky table

	ld hl, _blocky
	ld d, 0
	ld e, a
	add hl, de
	ld a, (hl)

	ld (ix+0), a			; Write cell to screen
	inc ix				; Skip attributes
	inc hl
	ld a, (hl)
	ld (ix+0), a			; Write attr to screen
	inc ix

	pop hl				; Restore next cluster address
	djnz _update_row

	; If we aren't using the whole screen for blocky graphics we need
	; to handle row skew here...

	;pop ix				; If width != screen uncomment this
	ld de, 64			; Skip odd rows
	add hl, de			;
	pop bc				; Number of blocks vertically
	djnz _update_nextrow
	pop ix
	ret

_blocky:
	DW 08320h, 080b8h, 080b4h, 080bch
	DW 080b2h, 080bah, 080b6h, 080beh
	DW 080b1h, 080b9h, 080b5h, 080bdh
	DW 080b3h, 080bbh, 080b7h, 08b20h
	

_app_main:
	call _clear_screen

	call _update_blocky

	ld a, _scrattr_ascii_n		; Normal Text
	ld (_text_attr), a
	ld a, 001h			; Line 15
	ld (_cur_y), a
	ld a, 001h			; Column 1 (Left)
	ld (_cur_x), a

_main_loop:
	
	call _getkey_wait

	cp _key_exit
	jr z, _exit_prompt

	jr _main_loop

_exit_prompt:
	call _clear_screen

	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a
	ld a, 008h				; Line 1 (Top)
	ld (_cur_y), a
	ld a, 002h				; Column 1 (Left)
	ld (_cur_x), a

	ld hl, _str_exit
	call _writestring

_wait_exit:
	call _getkey_wait
	cp 'y'
	jr z, _real_exit
	cp 'Y'
	jr z, _real_exit

	cp 'n'
	jr z, _app_main
	cp 'N'
	jr z, _app_main

	jr _wait_exit

_real_exit:
	call _clear_screen

	jp 014d5h				; Return to main menu.

include "lib/string.asm"
include "lib/screen.asm"
include "lib/printf.asm"
include "lib/keyb.asm"

gfx_screen:
include "lib/logo.asm"

_code_end:
;; End of Main Application

;; Fill to end of file
	org 0c0ffh
	seek 020ffh
	defb 000h
_file_end:
