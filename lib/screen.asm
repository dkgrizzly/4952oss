
_clear_screen:
	push de
	push hl
	push bc
	push af
	ld de, 04000h			; Screen Buffer
	ld hl, 04000h
	ld a, 020h				; ' '
	ld (hl), a
	inc hl
	ld a, 083h				; Normal Attribute
	ld (hl), a
	inc hl
	ex de,hl
	ld bc, 003feh
	ldir
	pop af
	pop bc
	pop hl
	pop de
	ret

_backspace:
	push af
	ld a,(_cur_x)
	dec a
	jr nz,_wrcurx
	ld a,01h
_wrcurx:
	ld (_cur_x),a
	pop af
	ret

; Print string at _cur_y, _cur_x, using _text_attr
; HL = Message (zero-terminated)
_writestring:
	ld a,(hl)				; Get character at HL
	or a					; Set flags
	ret z					; Return if zero
	call _writechar			; Print character from A
	inc hl					; Advance
	jr _writestring			; Loop

; Print char at _cur_y, _cur_x, using _text_attr
; A = Character
_writechar:
	cp 00ah					; Newline?
	jr z,_advance_line_nl	;
	cp 00dh					; Carriage Return?
	jr z,_advance_line		;
	cp 00ch					; Clear Screen?
	jr z,_clear_screen		;
	cp 008h					; Backspace?
	jr z,_backspace			;
	push de					; Store clobbered registers
	push hl					;
	push af					;

	ld h,000h
	ld a,(_cur_y)			; Line
	ld l,a
	dec l					; --
	add hl,hl				; <<
	add hl,hl				; <<
	add hl,hl				; <<
	add hl,hl				; <<
	add hl,hl				; <<
	add hl,hl				; <<

	ex de,hl				; de = hl

	ld h,000h
	ld a,(_cur_x)			; Column
	dec a					; --
	add a,a					; <<
	ld l,a					; 
	add hl,de				;

	ld de,04000h			; Screen Buffer
	add hl,de				;

	pop af					; 
	ld (hl),a				; Text Character
	inc hl					;

	ld a,(_text_attr)		;
	ld (hl),a				; Text Attribute

	pop hl					; Restore clobbered registers
	pop de					;

_advance_cursor:
	ld a,(_cur_x)
	inc a					; Advance cursor
	ld (_cur_x),a
	cp 021h					; Should we wrap?
	ret m					; nope

	jr _advance_line

_advance_line_nl:			; Newline only advances if X != 1
	ld a,(_cur_x)			; else we assume last char was CR?
	cp 001h
	ret z

_advance_line:
	ld a,001h				; Wrap to next line
	ld (_cur_x),a
	ld a,(_cur_y)
	inc a
	ld (_cur_y),a
	cp 11h
	ret m

	push hl
	push de
	push bc
	; Scroll buffer
	ld hl, 04040h
	ld de, 04000h
	ld bc, 003c0h
	ldir
	pop bc
	pop de
	pop hl

	ld a,010h				; We ran out of lines!
	ld (_cur_y),a			; loop (for now)
	ret						;

_cur_x:
	defb 001h
_cur_y:
	defb 001h
_text_attr:
	defb 083h
