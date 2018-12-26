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

	defb 007h, 00ch, _scrattr_ascii_n
	defb "Port Test", 000h

	defb 00ch, 008h, _scrattr_ascii_n
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, _scrattr_ascii_n
	defb "on hackaday.io", 000h

	defb 000h			;; End of Screen Data

_splash_menu_data:
	defb "Re-!BERT!Remote!Mass !Port!Self~"
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
	jp _app_main			; Run the application

_splash_end:
;; End of menu section

;; Main Application
	org 2200h
	seek 0a00h
_code_start:

_cnt_frames:
	defw 00800h

_str_frames:
	defb "%d %d    ", 000h
	
_str_finished:
	defb "Goodbye.", 000h

_str_running:
	defb "Running...", 000h

; This table has the Y, X, and Port number to display
_screen_porttest:
	defb 001h, 001h, 000h
	defb 001h, 006h, 001h
	defb 001h, 00bh, 002h
	defb 001h, 010h, 003h
	defb 001h, 015h, 009h
	defb 001h, 01ah, 00dh

	defb 003h, 001h, 010h
	defb 003h, 006h, 011h
	defb 003h, 00bh, 015h
	defb 003h, 010h, 016h

	defb 005h, 001h, 020h
	defb 005h, 006h, 021h
	defb 005h, 00bh, 022h
	defb 005h, 010h, 023h
	defb 005h, 015h, 029h
	defb 005h, 01ah, 02fh

	defb 007h, 001h, 030h
	defb 007h, 006h, 031h
	defb 007h, 00bh, 032h
	defb 007h, 010h, 03fh
	defb 007h, 01ah, 040h

	defb 009h, 001h, 050h
	defb 009h, 006h, 051h
	defb 009h, 00bh, 05ch
	defb 009h, 010h, 05dh
	defb 009h, 015h, 05eh
	defb 009h, 015h, 05fh

	defb 00bh, 001h, 062h
	defb 00bh, 006h, 063h
;	defb 00bh, 00bh, 070h
;	defb 00bh, 010h, 078h
	defb 00bh, 015h, 0a3h
	defb 00bh, 01ah, 0cdh

	defb 000h				;; End of Data

_str_hex:
	defb " %x ", 000h

_str_exit:
	defb "Are you sure you wish to exit?", 000h

_app_main:
	call _clear_screen

	ld a, _scrattr_ascii_i			; Inverse Text
	ld (_text_attr), a

	ld hl, _screen_porttest
_next_label:
	ld a, (hl)
	cp 000h
	jr z, _main_loop

	ld (_cur_y), a
	inc hl		
	ld a, (hl)
	ld (_cur_x), a
	inc hl		

	ld a, (hl)

	inc hl
	push hl

	ld d, 000h
	ld e, a
	push de

	ld hl, _str_hex
	push hl
	call _printf

	pop hl
	jp _next_label



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


_main_loop:
	call _keyscan
	call _getkey_nowait
	cp _key_exit
	jr z, _exit_prompt

	ld a, _scrattr_ascii_n			; Normal Text
	ld (_text_attr), a

	ld hl, _screen_porttest
_next_port:
	ld a, (hl)
	cp 000h
	jr z, _main_loop			; While(Y != 0) {
	inc a
	ld (_cur_y), a				; Set Y Coordinate + 1
	inc hl		
	ld a, (hl)
	ld (_cur_x), a				; Set X Coordinate
	inc hl		

	ld a, (hl)				; Get Port Number
	inc hl
	push hl

	ld c, a					; Read Port
	in a,(c)

	ld d, 000h				; Print Value
	ld e, a
	push de

	ld hl, _str_hex
	push hl
	call _printf

	pop hl					; And move on.
	jp _next_port

include "LIB/screen.asm"
include "LIB/printf.asm"
include "LIB/keyb.asm"

_code_end:
;; End of Main Application

;; Fill to end of file
	org 0b0ffh
	seek 010ffh
	defb 000h
_file_end:
