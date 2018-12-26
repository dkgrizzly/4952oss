include "lib/header.asm"
include "lib/strap.asm"

	org 02071h
	seek 00871h
_splash_screen_data:
	defb 0ffh

	defb 002h, 00dh, _scrattr_ascii_n
	defb "HP 4952A", 000h
	defb 003h, 007h, _scrattr_ascii_n
	defb "Open Source Software", 000h

	defb 007h, 00bh, _scrattr_ascii_n
	defb "Keyboard Test", 000h

	defb 00ch, 008h, _scrattr_ascii_n
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, _scrattr_ascii_n
	defb "on hackaday.io", 000h

	defb 000h							;; End of Screen Data

_splash_menu_data:
	defb "Re-!BERT!Remote!Mass !Key !Self~"
	defb "set!Menu!&Print!Store!Test!Test|"

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
	jp _main_entry			; Run the application

_splash_end:
;; End of menu section

;; Main Application
	org 2200h
	seek 0a00h
_code_start:

_str_frames:
	defb "%d %d    ", 000h
	
_str_finished:
	defb "Goodbye.", 000h

_str_running:
	defb "Running...", 000h
	
_str_hex:
	defb "%x", 000h

_str_blank:
	defb "  ", 000h

_str_exit:
	defb "Are you sure you wish to exit?", 000h


_spc_keys:
	cp 0ffh
	jr z, _no_keys

	ld h, 000h
	ld l, a
	push hl
	ld a, 005h				; Line 5 (Top)
	ld (_cur_y), a
	ld a, 010h				; Column 16 (Left)
	ld (_cur_x), a
	ld hl, _str_hex
	push hl
	call _printf
	jr _main_loop

_no_keys:
	ld a, 005h				; Line 5 (Top)
	ld (_cur_y), a
	ld a, 010h				; Column 16 (Left)
	ld (_cur_x), a
	ld hl, _str_blank
	call _writestring
	jr _main_loop

_main_entry:
	call _clear_screen

_main_loop:
	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a

	call _keyscan

	ld hl, _keystates + _scancode_exit
	ld a, (hl)
	cp 007h
	jr z, _exit_prompt

	call _show_matrix

	call _getkey_nowait
	bit 7,a
	jr nz, _spc_keys
	cp 020h
	jp m, _spc_keys

	push af
	ld a, 005h				; Line 15 (Top)
	ld (_cur_y), a
	ld a, 010h				; Column 16 (Left)
	ld (_cur_x), a
	pop af
	call _writechar_raw

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
	call _keyscan

	call _getkey_wait
	cp 'y'
	jr z, _real_exit
	cp 'Y'
	jr z, _real_exit

	cp 'n'
	jr z, _main_entry
	cp 'N'
	jr z, _main_entry

	jr _wait_exit

_real_exit:
	call _clear_screen

	jp 014d5h				; Return to main menu.

_show_matrix:
	ld a, _scrattr_hextex_n			; HexaText
	ld (_text_attr), a

	ld hl, _keystates
	ld b, 008h
	ld a, 005h
	ld (_cur_y), a
_next_row:
	ld a, 005h
	ld (_cur_x), a

	ld c, b					; save outer loop count
	ld b, 008h
_next_col:
	ld a, (hl)
	call _writechar_raw

_check_next_col:
	inc hl
	djnz _next_col

	ld b, c					; restore outer loop count

	ld a, (_cur_y)
	inc a
	ld (_cur_y), a

	djnz _next_row

	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a
	ret

include "lib/string.asm"
include "lib/screen.asm"
include "lib/printf.asm"
include "lib/keyb.asm"

_code_end:

;; End of Main Application

;; Fill to end of file
	org 0b0ffh
	seek 010ffh
	defb 000h
_file_end:
