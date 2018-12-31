bfi_reset:
    ld HL, BF_TMP_SP
    ld (hl), $00
    ld HL, BF_ARRAY
    ld B, H
    ld C, L
    ld HL, BF_PTR                       ; Reset pointer
    ld (hl), B
    inc HL
    ld (hl), C
    ld HL, BF_CHAR                      ; Reset temp char
    ld (hl), $00
    inc HL
    ld (hl), $00
    ld HL, BF_ARRAY
    ld DE, BF_ARRAY
	ld BC, BF_ARRAY_SIZE - 1
	ld (hl), $00
	inc hl
	ex de, hl
    ldir
    ret

bfi_syntax_error:
    ret

bfi_fde:                                ; BF Fetch-Decode-Execute
.bfi_fetch:                             ; HL = command buffer, should be null term
    ld A, (hl)                          ; Fetch instruction
    inc HL                              ; increment PC
    cp $00
    jr nz, .bfi_decode
    jp .bfi_fde_end                     ; if we are at the end we jump to the end
.bfi_decode:
    ld B, H
    ld C, L
    ld HL, BF_TMP_HL                    ; save the PC
    ld (hl), B
    inc HL
    ld (hl), C
    cp BF_INC_PTR
    jr z, .bfi_execute_inc_ptr
    cp BF_DEC_PTR
    jr z, .bfi_execute_dec_ptr
    cp BF_INC_DAT
    jr z, .bfi_execute_inc_dat
    cp BF_DEC_DAT
    jr z, .bfi_execute_dec_dat
    cp BF_OUT
    jr z, .bfi_execute_out
    cp BF_IN
    jp z, .bfi_execute_in
    cp BF_REP_STR
    jp z, .bfi_execute_rep_start
    cp BF_REP_END
    jp z, .bfi_execute_rep_end
    ld HL, BF_TMP_HL
    ld B, (hl)
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    jr .bfi_fetch
.bfi_execute_inc_ptr:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    inc BC                              ; increment pointer copy
    dec HL
    ld (hl), B
    inc HL
    ld (hl), C                          ; save copy of pointer
    jp .bfi_restore_pc
.bfi_execute_dec_ptr:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    dec BC                              ; decrement pointer copy
    dec HL
    ld (hl), B
    inc HL
    ld (hl), C                          ; save copy of pointer
    jp .bfi_restore_pc
.bfi_execute_inc_dat:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    inc (hl)                            ; increment data at pointer
    jp .bfi_restore_pc
.bfi_execute_dec_dat:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    dec (hl)                            ; decrement data at pointer
    jp .bfi_restore_pc
.bfi_execute_out:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    ld A, (hl)
    call _writechar
    jr .bfi_restore_pc
.bfi_execute_in:
    call _getkey_wait
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    ld (hl), A
    jr .bfi_restore_pc                  ; no support for input yet
    nop
.bfi_execute_rep_start:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    ld A, (hl)
    cp $00                              ; check if data in ptr is $00
    jr z, .bfi_wait_rep_end             ; if it's $00 then we continue
    ld HL, BF_TMP_SP
    inc (hl)
    ld HL, BF_TMP_HL
    ld B, (hl)
    inc HL
    ld C, (hl)
    dec BC
    push BC
    jr .bfi_restore_pc
.bfi_execute_rep_end:
    ld HL, BF_PTR
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    ld A, (hl)
    cp $00
    jr nz, .bfi_execute_rep_end_continue
    ld HL, BF_TMP_SP
    ld A, (hl)
    cp $00
    jr z, .bfi_restore_pc
    pop BC
    dec (hl)
    jr .bfi_restore_pc
.bfi_execute_rep_end_continue:
    pop BC
    ld HL, BF_TMP_SP
    dec (hl)
    ld HL, BF_TMP_HL
    ld (hl), B
    inc HL
    ld (hl), C
    jr .bfi_restore_pc
.bfi_wait_rep_end:
    ld HL, BF_TMP_HL
    ld B, (hl)                          ; dereference pointer
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
.bfi_wait_rep_end_loop:
    ld A, (hl)
    inc HL
    cp BF_REP_END
    jr nz, .bfi_wait_rep_end_loop
    ld B, H
    ld C, L
    ld HL, BF_TMP_HL
    ld (hl), B
    inc HL
    ld (hl), C
.bfi_restore_pc:
    ld HL, BF_TMP_HL
    ld B, (hl)
    inc HL
    ld C, (hl)
    ld H, B
    ld L, C
    jp .bfi_fetch
.bfi_fde_end:
    ret

bfi_error:
    ret

org 0c000h
BF_ARRAY: DS $0C00                      ; 3 KB of memory
BF_PTR: DS $02                          ; 16 bit pointer
BF_CHAR: DS $02
BF_TMP_HL: DS $02
BF_TMP_SP: DS $01

BF_ARRAY_SIZE:   EQU $0BFF
BF_INC_PTR:      EQU '>'
BF_DEC_PTR:      EQU '<'
BF_INC_DAT:      EQU '+'
BF_DEC_DAT:      EQU '-'
BF_OUT:          EQU '.'
BF_IN:           EQU ','
BF_REP_STR:      EQU '['
BF_REP_END:      EQU ']'

