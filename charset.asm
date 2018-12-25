include "lib/header.asm"
include "lib/strap.asm"

	org 02071h
	seek 00871h
_splash_start:
_splash_screen_data:
	defb 0ffh

	defb 002h, 00dh, _scrattr_ascii_n
	defb "HP 4952A", 000h
	defb 003h, 007h, _scrattr_ascii_n
	defb "Open Source Software", 000h

	defb 007h, 009h, _scrattr_ascii_n
	defb "Charset Explorer", 000h

	defb 00ch, 008h, _scrattr_ascii_n
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, _scrattr_ascii_n
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

;; Main Application
	org 2200h
	seek 0a00h
_code_start:
_app_main:
	call _clear_screen

_redraw:
	xor a
_next_row:
	ld c, a
	inc a					; Set Y coordinate
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a
	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a

	sla c
	sla c
	sla c
	sla c

	ld b, 0					; Print row starting charnum
	push bc
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
	ld a, (_cur_y)				; Set starting char for row
	dec a
	sla a
	sla a
	sla a
	sla a
_next_char:
	push af
	call _writechar_raw
	pop af
	inc a
	djnz _next_char

	ld a, _scrattr_graphics			; Border Text
	ld (_text_attr), a
	ld a, 09bh				; Left Edge
	call _writechar_raw

	ld a, (_cur_y)				; Repeat for 16 rows
	cp 010h
	jr c, _next_row

	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 1
	ld (_cur_y), a
	ld a, 019h				; Column 25
	ld (_cur_x), a

	ld a, (_cur_attr)			; Print current text attribute
	ld b, 0
	ld c, a
	push bc
	ld hl, _str_hex
	push hl
	call _printf

	ld a, (_attr_bit)			; Print current text attribute bit
	ld b, 0
	ld c, a
	push bc
	ld hl, _str_hex
	push hl
	call _printf

_main_loop:

	call _getkey_wait

	cp _key_up
	jr z, _next_attr
	cp _key_dn
	jr z, _prev_attr

	cp _key_lt
	jr z, _next_bit
	cp _key_rt
	jr z, _prev_bit

	cp _key_f1
	jr z, _set_ascii_n
	cp _key_f2
	jr z, _set_ascii_i

	cp _key_f3
	jr z, _set_ebcdic_n
	cp _key_f4
	jr z, _set_ebcdic_i

	cp _key_f5
	jr z, _set_hextex_n
	cp _key_f6
	jr z, _set_hextex_i

	cp _key_exit
	jr z, _exit_prompt

	jr _main_loop
	
_set_ascii_n:
	ld a, _scrattr_ascii_n
	jr _set_attr

_set_ascii_i:
	ld a, _scrattr_ascii_i
	jr _set_attr

_set_ebcdic_n:
	ld a, _scrattr_ebcdic_n
	jr _set_attr

_set_ebcdic_i:
	ld a, _scrattr_ebcdic_i
	jr _set_attr

_set_hextex_n:
	ld a, _scrattr_hextex_n
	jr _set_attr

_set_hextex_i:
	ld a, _scrattr_hextex_i
_set_attr:
	ld (_cur_attr), a
	jp _redraw

_next_attr:
	ld a, (_attr_bit)	; Up and Down arrows increase or decrease attribute by selected bits
	ld c, a
	ld a, (_cur_attr)
	add c
	ld (_cur_attr), a
	jp _redraw
_prev_attr:
	ld a, (_attr_bit)
	ld c, a
	ld a, (_cur_attr)
	sub c
	ld (_cur_attr), a
	jp _redraw

_next_bit:
	ld a, (_attr_bit)	; Left and Right arrows select the bit value to add/subtract
	rlca
	ld (_attr_bit), a
	jp _redraw
_prev_bit:
	ld a, (_attr_bit)
	rrca
	ld (_attr_bit), a
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

_str_exit:
	defb "Are you sure you wish to exit?", 000h

_str_row:
	defb "%x", 000h
	
_str_hex:
	defb "%x", 000h

_cur_attr:
	defb _scrattr_ascii_n

_attr_bit:
	defb 001h

include "lib/delay.asm"
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
