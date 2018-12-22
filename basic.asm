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
	defb "4952 Basic                      "

_filesize:
	org 0a102h
	seek 00102h
	defw 00030h								; Blocks in file - 1

	defb " HP4952 BASIC     4952  "
	defw 00800h
_fileflags:
	defw 00000h							; Flags 0200h is copy protect

	defb "4952 Basic                      "

;; Entry Point
	org 0a147h
	seek 00147h
	defb 0000h
_entryaddr:
	defw __init

;; Main Application
	org 0a150h
	seek 00150h
	defw _app_loader

;; ???
	org 0a17eh
	seek 0017eh
	defw 0f958h

;; Dynamic link loader data pointer & size
	org 0a180h
	seek 00180h
	defw (__dll_fixups_end - __dll_fixups) / 6 ; Number of patches
	defw __dll_fixups			; Location of patches

__init:
	di					; Disable Interrupts
	call _load_dll_stub	; Call our dynamic linker

	ld de,02000H				;
	ld hl,0a800H				; Load menu data & stubs
	ld bc,00200H				;
	ldir				;

	jp 02000h			; Run main menu stub

__0a196h:
	ld hl,0a800H				;
	ld de,02000H				; Load menu data & stubs again?
	ld bc,00200H				;
	ldir				;

	ld hl,07621H				;
	push hl				;
	ld a,002h			; Patch function at 1109?
	ld (0110ch),a		;
	ld hl,0d966H				;
	ld (0110dh),hl		;
	call 01109h			;

__0a1b3h:
	ld hl,0a800H				;
	ld de,02000H				; Load menu data & stubs again?
	ld bc,00200H				;
	ldir				;

	ld hl,0761cH				;
	ex (sp), hl			;
	ld a,002h			; Patch function at 1109?
	ld (0110ch),a		;
	ld hl,0d966H				;
	ld (0110dh),hl		;
	call 01109h			;

	jp __0a1b3h			; Loop Forever
	ret					; How can we ever get here?

;; This is a dynamic linker, at runtime it loads a copy of the
;; ROM vector table into RAM and fixes up all the stubbed
;; ROM references in the executable

;; Load and execute ordinal patching stub from a safe location
_load_dll_stub:
	ld hl,0a210H				;
	ld de,02a00H				;
	ld bc,00036H				;
	ldir				;
	call _dll_stub		;

	ld ix,(0a182h)		; Load patch table from ()
	ld bc,(0a180h)		; Load patch count
	ld l,(ix+000h)		;
	ld h,(ix+001h)		;
	ld e,(hl)			; Read LBYTE
	inc hl				;
	ld d,(hl)			; Read HBYTE
	ld l,(ix+002h)		; Get Patch Value
	ld h,(ix+003h)		;
	add hl,de			; Patch the pointer
	ex de,hl			;
	ld l,(ix+004h)		; Dest Address
	ld h,(ix+005h)		;
	ld (hl),e			; Write LBYTE
	inc hl				;
	ld (hl),d			; Write HBYTE
	ld de,00006H				;
	add ix,de			; Next Entry
	dec bc				;
	ld a,b				;
	or c				;
	jr nz,$-34			; More entries?
	ret					;

;; Local temp index variable
_dll_tmp:
	defb 000h

;; 54bytes - Relocated at runtime to 02a00h and executed
	org 02a00h
	seek 00210h

_dll_stub:
	ld a,004h			; Access Page 4 - 10046 ROM Lower Page
	out (020h),a		;
	ld hl,08000H				; Copy system ordinals from 10046 ROM
	ld de,02d00H				;
	ld bc,00134H				;
	ldir				;
	ld a,002h			; Access Page 2 - Application "ROM"
	out (020h),a		;

	ld hl,(02d0ch)		; Generate 17 more for 02e34h = 0d9f0h...0da20h
	ld bc,00003H				;
	ld a,011h			; .. Source appears to be a jump table
	call la246h			;

	ld hl,(02e16h)		; Generate 68 more for 02e56h =
	ld a,(hl)			;
	inc hl				; .. (some FM going on here...)
	ld h,(hl)			;
	ld l,a				;
	ld bc,00006H				;
	ld a,044h			;
	call 0a246h			;

	ld bc,00002H				; Generate 30 more for 02edeh = 0eb98h..
	ld a,01eh			;
	call la246h			;
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
__dll_fixups_end:

;; Relocated at runtime from 0a800h to 02000h
	org 02000h
	seek 00800h

	call 01543h			; Patched to 2d32 -> 01543h
	call 00fe9h			; Patched to 2e6e -> ????
	call 00085h			; Patched to 2d4a -> 00085h
	call l2065h			;
	ld hl,_splash		;200c	21 71 20 	! q   
	push hl				;
	call 01cf8h			; Patched to 2d50 -> 01cf8h
	pop hl				;
	call l2032h			;
	call 0007eh			; Patched to 2d6c -> 0007eh

	ld a,006h			; Load Page 6 (Application RAM)
	call 00e60h			; Patched to 2d02 -> 00e60h

	ld de,0a800H		;
	ld hl,02000H		; Load this section over again (dangerous)
	ld bc,00200H		;
	ldir				;

	ld a,002h			; Load Page 2 (10046 ROM)
	call 00e60h			; Patched to 2d02 -> 00e60h

	jp 014d5h			; Return via call -> 02e32h -> 014d5h

l2032h:
	ld a,002h			; Load Page 2 (10046 ROM)
	call 00e60h			;
	ld hl,_splash		;2037	21 71 20 	! q   
	ld (0761dh),hl		; Screen Paint Script Location
	ld hl,(0761fh)		; Copy main menu pointers
	ld de,_p_main_menu_page_one		; over the first page menu
	ld (0761fh),de		; pointers in our table
	ld (07624h),de		;
	ld bc,0000eH				;
	ldir				;

	ld a,(hl)			;
	inc hl				;
	ld h,(hl)			;
	ld l,a				;
	inc hl				; Skip to page two...
	inc hl				;
	ld de,_p_mm_reset	;
	ld bc,0000cH				;
	ldir				;

	ld hl,_app_loader	; Patch our application vector
	ld (_p_mm_playgame),hl		; for button five on page two
	ret					;

l2065h:
	ld a,006h			; Patch 00fd4h -> our menu display function
	ld (00fd4h),a		;
	ld hl,0a196H				; 
	ld (00fd5h),hl		;
	ret					;

	org 02071h
	seek 00871h
_splash:
	defb 0ffh

	defb 003h, 00dh, 083h
	defb "BASIC", 000h

	defb 006h, 009h, 083h
	defb "for the HP 4952", 000h
	defb 007h, 008h, 083h
	defb "Protocol Analyzer", 000h

;	defb 00ah, 008h, 083h, 0abh
;	defb " Copyright 1978", 000h
;	defb 00bh, 00ch, 083h
;	defb "Microsoft", 000h

	defb 000h							;; End of Screen Data

_splash_menu_data:
	defb "Re-!BERT!Remote!Mass !Run !Self~"
	defb "set!Menu!&Print!Store!Lang!Test|"

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
	defw _app_loader	;; Entry Point for Application
_p_mm_selftest:
	defw 0136fh			;; Ordinal 12ah Self Test
_p_mm_next2:
	defw _p_main_menu_page_one			;; Next Page

_app_loader:
	ld a, 006h
	call 00e60h			; Page in 6 - Really 2 (Application "ROM" in RAM)
	ld hl,0aa00H		; Copy application to Work RAM
	ld de,_CODE_START	;
	ld bc,_CODE_SIZE	;
	ldir				;
	jp _launch_app		; Run the application

;; End of menu section

;; Main Application
	org 2160h
	seek 0a00h
_CODE_START: equ $

_launch_app:
	call _clear_screen

	ld a, 083h				; Normal Text
	ld (_text_attr), a
	ld a, 001h				; Line 15
	ld (_cur_y), a
	ld a, 001h				; Column 1 (Left)
	ld (_cur_x), a

_main_loop:
	;ld a, 007h				; Page in 7 - Really 3 (Application ExRAM)
	;call 00e60h
	;di
	; jp STARTB
	;jp COLD

_return_to_rom:	
	jp 014d5h				; Return to main menu.

include "lib/screen.asm"
include "lib/keyb.asm"

;; End of Main Application

;; Fill to end of file
	org 0d0ffh
	seek 030ffh
	defb 000h
_file_end:
