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

	defb 007h, 009h, _scrattr_ascii_n
	defb "Memory Explorer", 000h

	defb 00ch, 008h, _scrattr_ascii_n
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, _scrattr_ascii_n
	defb "on hackaday.io", 000h

	defb 000h							;; End of Screen Data

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
_app_main:
	call _clear_screen
_redraw:
	call _read_page

	ld hl, _mem_buffer
	push hl
	xor a
_next_row:
	ld c, a
	inc a					; Set Y coordinate
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a
	ld a, 083h				; Normal Text
	ld (_text_attr), a

	ld b, 0					; Print row starting charnum
	sla c
	sla c
	sla c
	sla c
	push bc
	ld a, (_cur_page)
	ld l, a
 	push hl
	ld h, 0
	ld a, (_cur_bank)
	ld l, a
	push hl
	ld hl, _str_row
	push hl
	call _printf

	ld a, _scrattr_graphics			; Border Text
	ld (_text_attr), a
	ld a, 09ch				; Right Edge
	call _writechar_raw

	ld a, (_cur_attr)			; Selected Attribute Value
	ld (_text_attr), a

	ld b, 16
	pop hl
_next_char:
	ld a, (hl)
	inc hl
	call _writechar_raw
	djnz _next_char
	push hl

	ld a, _scrattr_graphics			; Border Text
	ld (_text_attr), a
	ld a, 09bh				; Left Edge
	call _writechar_raw

	ld a, (_cur_y)				; Repeat for 16 rows
	cp 010h
	jr c, _next_row

	pop hl
	
	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 1
	ld (_cur_y), a
	ld a, 019h				; Column 25
	ld (_cur_x), a

_main_loop:

	call _getkey_wait

	cp _key_up
	jr z, _next_page
	cp _key_dn
	jr z, _prev_page

	cp _key_rt
	jr z, _next_10page
	cp _key_lt
	jr z, _prev_10page

	cp _key_f1
	jr z, _set_ascii

	cp _key_f2
	jr z, _set_ebcdic

	cp _key_f3
	jr z, _set_hex

	cp _key_f4
	jr z, _set_inv

	cp _key_f5
	jr z, _prev_bank

	cp _key_f6
	jr z, _next_bank

	cp _key_exit
	jr z, _exit_prompt

	jr _main_loop
	
_set_ascii:
	ld a, _scrattr_ascii_n
	ld (_cur_attr), a
	jp _redraw

_set_ebcdic:
	ld a, _scrattr_ebcdic_n
	ld (_cur_attr), a
	jp _redraw

_set_hex:
	ld a, _scrattr_hextex_n
	ld (_cur_attr), a
	jp _redraw

_set_inv:
	ld a, (_cur_attr)
	xor _scrattr_inverse
	ld (_cur_attr), a
	jp _redraw

_next_bank:
	ld a,(_cur_bank)
	inc a
	ld (_cur_bank),a
	jp _redraw
_prev_bank:
	ld a,(_cur_bank)
	dec a
	ld (_cur_bank),a
	jp _redraw

_next_page:
	ld a,(_cur_page)
	inc a
	ld (_cur_page),a
	jp _redraw
_prev_page:
	ld a,(_cur_page)
	dec a
	ld (_cur_page),a
	jp _redraw

_next_10page:
	ld a,(_cur_page)
	add 010h
	ld (_cur_page),a
	jp _redraw
_prev_10page:
	ld a,(_cur_page)
	sub 010h
	ld (_cur_page),a
	jp _redraw


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
	jp z, _app_main
	cp 'N'
	jp z, _app_main

	jr _wait_exit

_real_exit:
	call _clear_screen

	jp 014d5h				; Return to main menu.


_read_page:
	di
	in a, (020h)			; save current bank
	ld (_tmp_bank), a

	ld a,(_cur_bank)		; access desired memory bank
	and 00fh
	out (020h),a

	ld l, 0				; Copy data to our buffer
	ld a, (_cur_page)
	or 080h
	ld h, a
	
	ld de, _mem_buffer
	ld bc, 00100h
	ldir

	ld a,(_tmp_bank)		; restore previous bank
	out (020h),a
;	ei
	ret

include "lib/string.asm"
include "lib/screen.asm"
include "lib/printf.asm"
include "lib/keyb.asm"

_str_exit:
	defb "Are you sure you wish to exit?", 000h

_str_row:
	defb "%x%x%x", 000h
	
;_str_hex:
;	defb "%x", 000h

_cur_attr:
	defb 083h
	
_cur_page:
	defb 000h

_cur_bank:
	defb 000h

_tmp_bank:
	defb 000h

_mem_buffer:
	defs 256, 000h

_code_end:
;; End of Main Application

;; Fill to end of file
	org 0b0ffh
	seek 010ffh
	defb 000h
_file_end:
