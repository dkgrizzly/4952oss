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

	defb 006h, 005h, 083h
	defb "Character Set Explorer", 000h

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
_p_mm_launch_app:
	defw _launch_app		;; Entry Point for Application
_p_mm_selftest:
	defw 0136fh			;; Ordinal 12ah Self Test
_p_mm_next2:
	defw _p_main_menu_page_one			;; Next Page

_launch_app:
	ld a, 006h
	call 00e60h			; Page in 6
	ld hl,0aa00h		; Copy application to Work RAM
	ld de,02160h		;
	ld bc,01e9fh		; Fixme: update to size of main code
	ldir				;
	jp _app_main		; Run the application

;; End of menu section

;; Main Application
	org 2160h
	seek 0a00h

_cnt_frames:
	defw 00800h

_str_finished:
	defb "Goodbye.", 000h

_str_hello:
	defb "    0123456789abcdef\n"
	defb "    ||||||||||||||||\n"
	defb "  8-"
	defb 080h, 081h, 082h, 083h, 084h, 085h, 086h, 087h
	defb 088h, 089h, 08ah, 08bh, 08ch, 08dh, 08eh, 08fh
	defb "\n"
	defb "  9-"
	defb 090h, 091h, 092h, 093h, 094h, 095h, 096h, 097h
	defb 098h, 099h, 09ah, 09bh, 09ch, 09dh, 09eh, 09fh
	defb "\n"
	defb "  a-"
	defb 0a0h, 0a1h, 0a2h, 0a3h, 0a4h, 0a5h, 0a6h, 0a7h
	defb 0a8h, 0a9h, 0aah, 0abh, 0ach, 0adh, 0aeh, 0afh
	defb "\n"
	defb "  b-"
	defb 0b0h, 0b1h, 0b2h, 0b3h, 0b4h, 0b5h, 0b6h, 0b7h
	defb 0b8h, 0b9h, 0bah, 0bbh, 0bch, 0bdh, 0beh, 0bfh
	defb "\n"
	defb "  c-"
	defb 0c0h, 0c1h, 0c2h, 0c3h, 0c4h, 0c5h, 0c6h, 0c7h
	defb 0c8h, 0c9h, 0cah, 0cbh, 0cch, 0cdh, 0ceh, 0cfh
	defb "\n"
	defb "  d-"
	defb 0d0h, 0d1h, 0d2h, 0d3h, 0d4h, 0d5h, 0d6h, 0d7h
	defb 0d8h, 0d9h, 0dah, 0dbh, 0dch, 0ddh, 0deh, 0dfh
	defb "\n"
	defb "  e-"
	defb 0e0h, 0e1h, 0e2h, 0e3h, 0e4h, 0e5h, 0e6h, 0e7h
	defb 0e8h, 0e9h, 0eah, 0ebh, 0ech, 0edh, 0eeh, 0efh
	defb "\n"
	defb "  f-"
	defb 0f0h, 0f1h, 0f2h, 0f3h, 0f4h, 0f5h, 0f6h, 0f7h
	defb 0f8h, 0f9h, 0fah, 0fbh, 0fch, 0fdh, 0feh, 0ffh
	defb "\n"
	defb 000h

_app_main:
	call _clear_screen

	ld a, 083h				; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 15
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a

	ld hl, _str_hello
	push hl
	call _printf


_main_loop:

	ld bc,01000h			; Delay 2048 delay counts
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
