.device ATmega8

.equ program_buffer = 0x0060 ; bottom of RAM

.cseg
.org 0x0000

    rjmp reset
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti
    reti


reset:

  ; setup stack pointer
  ldi r16, low(RAMEND)
  ldi r17, high(RAMEND)
  out SPL, r16
  out SPH, r17

  ; PB1 for debug
  ldi r16, (1<<PB1)
  out DDRB, r16


main:

  rcall create_program
  rcall execute_program
  rjmp 0


create_program:

  ; fill the program buffer with a compiled program
  ; XXX temporary until we have a working parser/loader

  ldi ZL, low(static_program_buffer*2)
  ldi ZH, high(static_program_buffer*2)

  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)

  lpm r16, Z+
  st X+, r16
  cpi r16, 0xff
  brne -3

  ret

static_program_buffer:
BL10:
  .dw (BL20-static_program_buffer)*2+program_buffer
  .dw 10
  .db 0x0c ; ON
BL20:
  .dw (BL30-static_program_buffer)*2+program_buffer
  .dw 20
  .db 0x0e ; SLEEP
BL30:
  .dw (BL40-static_program_buffer)*2+program_buffer
  .dw 30
  .db 0x0d ; OFF
BL40:
  .dw (BL50-static_program_buffer)*2+program_buffer
  .dw 40
  .db 0x0e ; SLEEP
BL50:
  .dw 0
  .dw 50
  .db 0x03, 0x0a, 0x00 ; GOTO 10

  .db 0xff


execute_program:

  ; set next line pointer to start of program buffer
  ldi r24, low(program_buffer)
  ldi r25, high(program_buffer)

execute_mainloop:

  ; if next line pointer is null, program is over
  or r24, r25
  breq execute_done

  ; prepare current line pointer
  mov XL, r24
  mov XH, r25

  ; hold location of next line in r24:r25
  ld r24, X+
  ld r25, X+

  ; skip the line number
  adiw X, 2

  ; statement now at X, execute it
  rcall execute_statement

  ; next line!
  rjmp execute_mainloop

  ; program done!
execute_done:
  ;rjmp blink_forever
  rjmp 0

;
; the Tiny BASIC (1975) grammar.
;
;    line ::= number statement CR | statement CR
;
;    statement ::= PRINT expr-list
;                  IF expression relop expression THEN statement
;                  GOTO expression
;                  INPUT var-list
;                  LET var = expression
;                  GOSUB expression
;                  RETURN
;                  CLEAR
;                  LIST
;                  RUN
;                  END
;
;    expr-list ::= (string|expression) (, (string|expression) )*
;
;    var-list ::= var (, var)*
;
;    expression ::= (+|-|ε) term ((+|-) term)*
;
;    term ::= factor ((*|/) factor)*
;
;    factor ::= var | number | (expression)
;
;    var ::= A | B | C ... | Y | Z
;
;    number ::= digit digit*
;
;    digit ::= 0 | 1 | 2 | 3 | ... | 8 | 9
;
;    relop ::= < (>|=|ε) | > (<|=|ε) | =
;
;    string ::= " (a|b|c ... |x|y|z|A|B|C ... |X|Y|Z|digit)* "
;

; run the statement at X
execute_statement:

  ; setup op table pointer
  ldi ZL, low(op_table)
  ldi ZH, high(op_table)

  ; opcode at X is offset into the op table
  ld r16, X+

  ; add op table location
  clr r17
  add ZL, r16
  adc ZH, r17

  ; and jump to the handler. it will return to our caller
  ijmp

; op table. opcodes are just indexes into this table, which then point off
; to the routine that executes that opcode
;
op_table:
  .dw 0           ; 0x00 [reserved]
  rjmp op_print   ; 0x01 PRINT expr-list
  rjmp op_if      ; 0x02 IF expression relop expression THEN statement
  rjmp op_goto    ; 0x03 GOTO expression
  rjmp op_input   ; 0x04 INPUT var-list
  rjmp op_let     ; 0x05 LET var = expression
  rjmp op_gosub   ; 0x06 GOSUB expression
  rjmp op_return  ; 0x07 RETURN
  rjmp op_clear   ; 0x08 CLEAR
  rjmp op_list    ; 0x09 LIST
  rjmp op_run     ; 0x0a RUN
  rjmp op_end     ; 0x0b END
  rjmp op_on      ; 0x0c [ON]
  rjmp op_off     ; 0x0d [OFF]
  rjmp op_sleep   ; 0x0e [SLEEP]

op_print:
  ret

op_if:
  ret

op_goto:
  ; target line
  ld r16, X+
  ld r17, X+

  ; get pointer to start of program buffer
  ldi r18, low(program_buffer)
  ldi r19, high(program_buffer)

op_goto_search_loop:

  ; if next line pointer is null, search is over
  or r18, r19
  breq op_goto_search_failed

  ; prepare current line pointer
  mov XL, r18
  mov XH, r19

  ; take location of next line to r18:r19
  ld r18, X+
  ld r19, X+

  ; move line number to r20:r21
  ld r20, X+
  ld r21, X+

  ; compare current line number in r20:r21 with the wanted one in r16:r17
  cp r20, r16
  brne op_goto_search_loop
  cp r21, r17
  brne op_goto_search_loop

  ; found it, set the next line pointer for execution to here
  ldi r16, 4
  clr r17
  sub XL, r16
  sbc XH, r17
  mov r24, XL
  mov r25, XH

  ; return from command; mainloop will continue at the line we set
  ret

op_goto_search_failed:
  ; XXX abort program
  rjmp blink_forever


op_input:
  ret

op_let:
  ret

op_gosub:
  ret

op_return:
  ret

op_clear:
  ret

op_list:
  ret

op_run:
  ret

op_end:
  rjmp blink_forever
  ret

op_on:
  sbi PORTB, PB1
  ret

op_off:
  cbi PORTB, PB1
  ret

op_sleep:
  ; 2s
  ldi  r18, 163
  ldi  r19, 87
  ldi  r20, 3
  dec  r20
  brne -1
  dec  r19
  brne -3
  dec  r18
  brne -5
  ret


blink_forever:

  sbi PORTB, PB1

  ; ~500ms
  ldi  r18, 41
  ldi  r19, 150
  ldi  r20, 128
  dec  r20
  brne -1
  dec  r19
  brne -3
  dec  r18
  brne -5

  cbi PORTB, PB1

  ; ~500ms
  ldi  r18, 41
  ldi  r19, 150
  ldi  r20, 128
  dec  r20
  brne -1
  dec  r19
  brne -3
  dec  r18
  brne -5

  rjmp blink_forever
