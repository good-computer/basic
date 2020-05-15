;.device ATmega8
.include "m8def.inc"

; XXX I wonder if there's a better way to set up a memory map
.equ input_buffer     = SRAM_START
.equ input_buffer_end = input_buffer + 0x7f

.equ program_buffer   = input_buffer_end + 1


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

  ; usart tx/rx enable
  ldi r16, (1<<RXEN | 1<<TXEN)
  out UCSRB, r16

  ; usart frame config: 8N1 (8 data bits => UCSZ2:0 = 011)
  ldi r16, (1<<URSEL) | (1<<UCSZ0) | (1<<UCSZ1)
  out UCSRC, r16

  ; usart 38400 baud at 16MHz => UBRR = 25
  ldi r16, 25
  ldi r17, 0
  out UBRRL, r16
  out UBRRH, r17

  ; PB1 for debug
  ldi r16, (1<<PB1)
  out DDRB, r16


main:

  ldi ZL, low(text_banner*2)
  ldi ZH, high(text_banner*2)
  rcall usart_print


main_loop:

  ldi ZL, low(text_prompt*2)
  ldi ZH, high(text_prompt*2)
  rcall usart_print

  rcall usart_line_input

  ;ldi ZL, low(input_buffer)
  ;ldi ZH, high(input_buffer)
  ;ldi r16, 0x80
  ;rcall usart_tx_bytes_hex

  rcall parse_line

  rjmp main_loop


  rcall create_program
  rcall execute_program
  rjmp 0


parse_line:

  ; start of buffer
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

  ; return if buffer is empty
  ; XXX ignore spaces
  ld r16, X
  or r16, r16
  brne PC+2
  ret

  rcall parse_number
  brvc PC+2
  rjmp blink_forever

  ; no line number? do the immediate mode thing
  brtc immediate_mode

  ; line number, set up to parse and store the statement
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  st X+, r2
  st X+, r3

  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  ldi r16, 0x2
  rjmp usart_tx_bytes_hex

immediate_mode:
  rcall parse_statement

  ret


parse_statement:

  ; take copy of pointer to start of statement, so we can reset it
  mov r2, XL
  mov r3, XH

  ; start of statement table
  ldi ZL, low(statement_table*2)
  ldi ZH, high(statement_table*2)

  ; walk both strings, comparing as we go. if we fail a compare, reset X, jump
  ; Z forward to next string
statement_loop:
  lpm r17, Z+

  ; if its below the ascii caps area, then its an opcode and we matched
  cpi r17, 0x40
  brlo statement_end

  ; load the next char of the input statement and compare
  ld r16, X+
  cp r16, r17
  breq statement_loop

  ; chars didn't match, so we have to start over on the next statement

  ; reset X to start of input statement
  mov XL, r2
  mov XH, r3

  ; walk Z forward to the next statement
  lpm r17, Z+
  cpi r17, 0x40
  brsh PC-2

  rjmp statement_loop

statement_end:

  ; but if its zero, we hit the end of the statement table, so it wasn't found
  or r17, r17
  brne PC+2
  rjmp blink_forever ; XXX not found

  ; print the opcode
  mov r16, r17
  rjmp usart_tx_byte_hex

  

statement_table:
  .db "PRINT",  0x1, \
      "IF",     0x2, \
      "GOTO",   0x3, \
      "INPUT",  0x4, \
      "LET",    0x5, \
      "GOSUB",  0x6, \
      "RETURN", 0x7, \
      "CLEAR",  0x8, \
      "LIST",   0x9, \
      "RUN",    0xa, \
      "END",    0xb, \
                     \
      "ON",     0xc, \
      "OFF",    0xd, \
      "SLEEP",  0xe, \
                     \
      0


; parse an ascii number
; inputs:
;   X: pointer to ascii digit sequence, will be moved
; outputs:
;   r2: low byte of number
;   r3: high byte of number
;   T:  set if we actually parsed something
;   V:  set if we overflowed 16 bits
parse_number:

  ; store X in case we need to rewind
  mov r7, XL
  mov r8, XH

  ; clear result flags
  clt
  clv

  ; accumulator
  clr r2
  clr r3

  ldi r18, 10 ; for multiplying by 10 repeatedly
  clr r19

parse_number_loop:
  ; get input char and check range
  ld r17, X+
  cpi r17, 0x30
  brsh PC+3

  ; out of range, roll X back one char and return
  ld r17, -X
  ret

  cpi r17, 0x3a
  brsh PC-3

  ; multiply accumulator low byte by 10
  mul r2, r18
  mov r5, r0
  mov r6, r1

  ; multiply accumulator high byte by 10
  mul r3, r18
  add r6, r0
  brcc PC+2
  inc r1
  cp r1, r19
  brne parse_number_overflow

  ; actually read something, flag this
  set

  ; reload the accumulator after multiplying
  mov r2, r5
  mov r3, r6

  ; convert digit to value, and add
  andi r17, 0xf
  add r2, r17
  brcc parse_number_loop
  inc r3
  brne parse_number_loop

  ; overflowed, restore X flag and abort
parse_number_overflow:
  mov XL, r7
  mov XH, r8
  sev
  ret



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
  brne PC-3

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
  adiw XL, 2

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
  brne PC-1
  dec  r19
  brne PC-3
  dec  r18
  brne PC-5
  ret


blink_forever:

  sbi PORTB, PB1

  ; ~500ms
  ldi  r18, 41
  ldi  r19, 150
  ldi  r20, 128
  dec  r20
  brne PC-1
  dec  r19
  brne PC-3
  dec  r18
  brne PC-5

  cbi PORTB, PB1

  ; ~500ms
  ldi  r18, 41
  ldi  r19, 150
  ldi  r20, 128
  dec  r20
  brne PC-1
  dec  r19
  brne PC-3
  dec  r18
  brne PC-5

  rjmp blink_forever


; receive a byte from the usart
; outputs:
;   r16: received byte
usart_rx_byte:
  sbis UCSRA, RXC
  rjmp PC-1

  in r16, UDR

  ret


; transmit a byte via the usart
; inputs:
;   r16: byte to send
usart_tx_byte:
  sbis UCSRA, UDRE
  rjmp PC-1

  out UDR, r16

  ret


; transmit a null-terminated string via the usart
; inputs:
;   Z: pointer to start of string in program memory
usart_print:
  lpm r16, Z+
  cpi r16, 0
  breq PC+3

  rcall usart_tx_byte
  rjmp usart_print

  ret


; receive a line of input into the input buffer, with simple editing controls
usart_line_input:

  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

uli_next_char:
  rcall usart_rx_byte

  ; printable ascii range is 0x20-0x7e
  ; XXX any computer made in 2020 needs to support unicode
  cpi r16, 0x20
  brlo uli_handle_control_char
  cpi r16, 0x7f
  brsh uli_handle_control_char

  ; something printable, make sure there's room in the buffer for it
  cpi XL, low(input_buffer_end)
  brne PC+3
  cpi XH, high(input_buffer_end)
  breq uli_next_char

  ; append to buffer and echo it
  st X+, r16
  rcall usart_tx_byte

  rjmp uli_next_char

uli_handle_control_char:

  ; enter/return
  cpi r16, 0x0d
  brne PC+2
  rjmp uli_do_enter

  ; delete/backspace
  cpi r16, 0x7f
  brne PC+2
  rjmp uli_do_backspace

  ; ignore everything else
  rjmp uli_next_char

uli_do_enter:
  ; zero end of buffer
  clr r16
  st X+, r16

  ; echo newline
  ldi r16, 0xa
  rcall usart_tx_byte
  ldi r16, 0xd
  rcall usart_tx_byte

  ; that's all the input!
  ret

uli_do_backspace:
  ; start-of-buffer check
  cpi XL, low(input_buffer)
  brne PC+3
  cpi XH, high(input_buffer)
  breq uli_next_char

  ; move buffer pointer back
  dec XL
  brpl PC+2
  dec XH

  ; echo destructive backspace
  ldi r16, 0x08
  rcall usart_tx_byte
  ldi r16, 0x20
  rcall usart_tx_byte
  ldi r16, 0x08
  rcall usart_tx_byte

  rjmp uli_next_char


; transmit a hex representation of a byte via the usart
; inputs:
;   r16: byte to send
usart_tx_byte_hex:
  push ZL
  push ZH

  push r16
  clr r17

  ldi ZL, low(hex_digits*2)
  ldi ZH, high(hex_digits*2)

  swap r16
  andi r16, 0x0f
  add ZL, r16
  adc ZH, r17
  lpm r16, Z
  rcall usart_tx_byte

  ldi ZL, low(hex_digits*2)
  ldi ZH, high(hex_digits*2)

  pop r16
  andi r16, 0x0f
  add ZL, r16
  adc ZH, r17
  lpm r16, Z
  rcall usart_tx_byte

  pop ZH
  pop ZL

  ret

hex_digits:
  .db "0123456789abcdef"


; transmit a hex representation of a block of data via the usart
; inputs:
;   Y: pointer to start of data in sram
;   r16: number of bytes to transmit
usart_tx_bytes_hex:
  mov r19, r16
  ldi r20, 8

  ldi r16, ' '
  rcall usart_tx_byte
  rcall usart_tx_byte

usart_tx_bytes_hex_next:
  ld r16, Z+
  rcall usart_tx_byte_hex

  dec r19
  breq usart_tx_bytes_hex_done

  ldi r16, ' '
  dec r20
  brne usart_tx_bytes_hex_print_gap

  ldi r20, 8

  ldi r16, 0xa
  rcall usart_tx_byte
  ldi r16, 0xd
  rcall usart_tx_byte
  ldi r16, ' '
  rcall usart_tx_byte

usart_tx_bytes_hex_print_gap:
  rcall usart_tx_byte
  rjmp usart_tx_bytes_hex_next

usart_tx_bytes_hex_done:
  ldi r16, 0xa
  rcall usart_tx_byte
  ldi r16, 0xd
  rcall usart_tx_byte

  ret


text_banner:
  .db 0xa, 0xd, "GOOD COMPUTER", 0xa, 0xd, 0
text_prompt:
  .db 0xa, 0xd, "BASIC> ", 0
