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
	defb "Character Attributes", 000h

	defb 00ch, 008h, 083h
	defb "Hacking the 4952", 000h
	defb 00dh, 009h, 083h
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
	ld de,02160h			;
	ld bc,01e9fh			; Fixme: update to size of main code
	ldir				;
	jp _app_main			; Run the application

;; Main Application
	org 2160h
	seek 0a00h

_cnt_frames:
	defw 00000h

_str_frames:
	defb "%x%x", 000h

_str_finished:
	defb "Goodbye.", 000h

_paint_attr:
	ld hl, 04000h
	ld d, 000h
	ld b, 0

_next_char:
	ld (hl), a				; Write character cell
	inc hl
	ld (hl), d				; Write attribute
	inc hl
	inc d					; Next attribute
	djnz _next_char
	ret

_app_main:
	call _clear_screen

_main_loop:
	ld a, (_cnt_frames+1)
	call _paint_attr

	ld a, 083h				; Normal Text
	ld (_text_attr), a
	ld a, 00eh				; Line 1 (Top)
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a

	ld hl, (_cnt_frames)
	ld d, 0
	ld e, h
	push de
	ld e, l
	push de
	ld hl, _str_frames
	push hl
	call _printf

	ld bc,00040h			; Delay 2048 delay counts
	call _delay

	ld hl, (_cnt_frames)	; Get frame count
	dec hl
	ld (_cnt_frames), hl	; Write it back
	ld a,h
	or l
	jr nz,_main_loop

	call _clear_screen

	ld a, 083h				; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 1 (Top)
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a

	ld hl, _str_finished
	call _writestring
	
	jp 014d5h				; Return to main menu.

include "lib/delay.asm"
include "lib/screen.asm"
include "lib/printf.asm"

;; End of Main Application

;; Fill to end of file
	org 0b0ffh
	seek 010ffh
	defb 000h
_file_end:
