_keyscan:
	push hl
	push de
	push af

	ld hl, _keystates

	ld a, 001h				; Row to scan
_k_scan_row:
	ld d, a
	out (0d0h), a			; Set Row Bits

	nop
	nop
	nop

	in a, (0d0h)			; Get Col Bits
	ld (hl), a
	inc hl

	ld a, d
	add a
	jr nz, _k_scan_row

	pop af
	pop de
	pop hl
	ret

; Wait for a key to be pressed and then released.
_getkey_cooked:
	push bc
	ld b, 0ffh
_gk_loop1:
	call _keyscan

	call _getkey_raw			; loop until we get a key
	cp b
	jr z, _gk_loop1
	ld b, a
_gk_loop2:
	call _keyscan

	call _getkey_raw			; and loop until it is released
	cp 0ffh
	jr nz, _gk_loop2
	ld a, b
	pop bc
	ret

_getkey_raw:
	push hl
	push de
	push bc


	ld hl, _keystates + 00006h
	ld a, (hl)
	bit 1,a
	jr nz, _gk_decode_c
	bit 2,a
	jr nz, _gk_decode_s
	ld de, _keymatrix_u
	jr _gk_first_row

_gk_decode_c:
	ld de, _keymatrix_c
	jr _gk_first_row

_gk_decode_s:
	ld de, _keymatrix_s

_gk_first_row:
	ld hl, _keystates
	ld b, 008h
_gk_next_row:
	ld a, (hl)
	inc hl
	ld c, a

	ld a, 001h
_gk_next_col:
	push af
	ex de, hl
	and c
	jr nz, _gk_decode
_gk_skip_code:
	ex de, hl
	pop af

	inc de
	add a
	jr nz,_gk_next_col

	djnz _gk_next_row

	or 0ffh							; No key pressed
	jr _gk_done

_gk_decode:
	ld a, (hl)
	or a
	cp 0ffh
	jr z, _gk_skip_code
	cp 0feh
	jr z, _gk_skip_code
	cp 0fdh
	jr z, _gk_skip_code

	pop bc							; Toss out our column number

_gk_done:
	pop bc
	pop de
	pop hl
	ret

_key_f1: equ 0e0h
_key_f2: equ 0e1h
_key_f3: equ 0e2h
_key_f4: equ 0e3h
_key_f5: equ 0e4h
_key_f6: equ 0e5h
_key_f7: equ 0e6h
_key_f8: equ 0e7h
_key_f9: equ 0e8h
_key_f10: equ 0e9h
_key_f11: equ 0eah
_key_f12: equ 0ebh

_key_more: equ 0efh
_key_help: equ 0edh

_key_exit: equ 0ech
_key_halt: equ 0eeh

_key_up: equ 0f6h
_key_dn: equ 0f7h
_key_lt: equ 0f8h
_key_rt: equ 0f9h

_key_pgup: equ 0f4h
_key_pgdn: equ 0f5h
_key_home: equ 0f2h
_key_end: equ 0f3h

_key_shift: equ 0fdh
_key_ctrl: equ 0feh

_keystates:
	defs 8, 000h

_keymatrix_u:
	defb 080h,  '^',  ']', '\\',  '[',  'z',  'y',  'x'		; 01
	defb  'w',  'v',  'u',  't',  's',  'r',  'q',  'p' 	; 02
	defb  'o',  'n',  'm',  'l',  'k',  'j',  'i',  'h'		; 04
	defb  'g',  'f',  'e',  'd',  'c',  'b',  'a',  '@'		; 08
	defb  '/',  '.',  '-',  ',',  ';',  ':',  '9',  '8'		; 10
	defb  '7',  '6',  '5',  '4',  '3',  '2',  '1',  '0'		; 20
	defb  ' ', 0feh, 0fdh, 00ah, 0f9h, 0f8h, 0f7h, 0f6h		; 40
	defb 0efh, 0e5h, 0e4h, 0e3h, 0e2h, 0e1h, 0e0h, 0ech		; 80

_keymatrix_s:
	defb 080h,  '~',  '}',  '|',  '{',  'Z',  'Y',  'X'		; 01
	defb  'W',  'V',  'U',  'T',  'S',  'R',  'Q',  'P' 	; 02
	defb  'O',  'N',  'M',  'L',  'K',  'J',  'I',  'H'		; 04
	defb  'G',  'F',  'E',  'D',  'C',  'B',  'A',  '`'		; 08
	defb  '/',  '>',  '=',  '<',  '+',  '*',  ')',  '('		; 10
	defb 027h,  '&',  '%',  '$',  '#',  '"',  '!',  '_'		; 20
	defb  ' ', 0feh, 0fdh, 00ah, 0f9h, 0f8h, 0f7h, 0f6h		; 40
	defb 0efh, 0ebh, 0eah, 0e9h, 0e8h, 0e7h, 0e6h, 0ech		; 80

_keymatrix_c:
	defb 080h, 01eh, 01dh, 01ch, 01bh, 01ah, 019h, 018h		; 01
	defb 017h, 016h, 015h, 014h, 013h, 012h, 011h, 010h 	; 02
	defb 00fh, 00eh, 00dh, 00ch, 00bh, 00ah, 009h, 008h		; 04
	defb 007h, 006h, 005h, 004h, 003h, 002h, 001h, 000h		; 08
	defb 01fh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh		; 10
	defb 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh, 0ffh		; 20
	defb 0a0h, 0feh, 0fdh, 00ah, 0f3h, 0f2h, 0f5h, 0f4h		; 40
	defb 0edh, 0e5h, 0e4h, 0e3h, 0e2h, 0e1h, 0e0h, 0eeh		; 80
