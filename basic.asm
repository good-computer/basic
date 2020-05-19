; vim: ft=avr

;.device ATmega8
.include "m8def.inc"

; XXX I wonder if there's a better way to set up a memory map

; stack 0400-045f
.equ stack_top        = RAMEND
.equ stack_bottom     = stack_top - 0x5f

; input buffer 0380-03ff
.equ input_buffer     = stack_bottom - 0x80
.equ input_buffer_end = stack_bottom - 1

; linked list of program instructions 0060->
.equ program_buffer     = SRAM_START
.equ program_buffer_end = input_buffer - 1

; global registers
.def r_error = r25
.def r_next_l = r23
.def r_next_h = r24
.def r_top_l = r22
.def r_top_h = r21

; error codes
.equ error_no_such_keyword     = 1
.equ error_number_out_of_range = 2
.equ error_expected_number     = 3
.equ error_no_such_line        = 4
.equ error_out_of_memory       = 5


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
  ldi r16, low(stack_top)
  ldi r17, high(stack_top)
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


  ; XXX DEBUG clear the program buffer
  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)
  ldi r16, low(program_buffer_end)
  ldi r17, high(program_buffer_end)
  ldi r18, 0x55
  ldi r19, 0xaa
  st X+, r18
  st X+, r19
  cp XL, r16
  cpc XH, r17
  brlo PC-4


  ; XXX this is CLEAR (NEW), I guess

  ; make first instruction zero-length, truncating the entire program
  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)
  clr r16
  st X+, r16

  ; set top pointer just past the zero-length instruction
  mov r_top_l, XL
  mov r_top_h, XH


main:

  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print
  ldi ZL, low(text_banner*2)
  ldi ZH, high(text_banner*2)
  rcall usart_print
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print


main_loop:

; XXX START DEBUG

;  ; start of program buffer
;  ldi ZL, low(program_buffer)
;  ldi ZH, high(program_buffer)
;  ldi r16, 0x20
;  ldi r17, 0x00
;  rcall usart_tx_bytes_hex
;
;  ldi r16, 0xa
;  rcall usart_tx_byte
;  ldi r16, 0xd
;  rcall usart_tx_byte
;
;  ; top of program buffer
;  ldi ZL, low(program_buffer_end-0x1f)
;  ldi ZH, high(program_buffer_end-0x1f)
;  ldi r16, 0x20
;  ldi r17, 0x00
;  rcall usart_tx_bytes_hex
;
;  ldi r16, 0xa
;  rcall usart_tx_byte
;  ldi r16, 0xd
;  rcall usart_tx_byte
;
;  ; start of input buffer (immediately after program buffer)
;  ldi ZL, low(input_buffer)
;  ldi ZH, high(input_buffer)
;  ldi r16, 0x20
;  ldi r17, 0x00
;  rcall usart_tx_bytes_hex

; XXX END DEBUG

  ; clear last error
  clr r_error

  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print
  ldi ZL, low(text_prompt*2)
  ldi ZH, high(text_prompt*2)
  rcall usart_print

  rcall usart_line_input

  rcall handle_line_input

  ; error check
  or r_error, r_error
  breq main_loop

  rcall handle_error
  rjmp main_loop




; unrecoverable error
handle_error:

  ; all error text starts with '?', oldschool
  ldi r16, '?'
  rcall usart_tx_byte

  ; make zero-based, and multiple for program memory lookup
  dec r_error
  lsl r_error

  ; point Z to error table entry
  ldi ZL, low(error_lookup_table*2)
  ldi ZH, high(error_lookup_table*2)
  add ZL, r_error
  brcc PC+2
  inc ZL

  ; done with error code, clear it
  clr r_error

  ; load error text location from table
  lpm XL, Z+
  lpm XH, Z+

  ; print it
  mov ZL, XL
  mov ZH, XH
  rcall usart_print

  ; and the newline
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rjmp usart_print

error_lookup_table:
  .dw text_error_no_such_keyword*2
  .dw text_error_number_out_of_range*2
  .dw text_error_expected_number*2
  .dw text_error_no_such_line*2
  .dw text_error_out_of_memory*2


skip_whitespace:
  ld r16, X
  cpi r16, 0x20
  breq PC+2
  ret
  adiw XL, 1
  rjmp skip_whitespace

handle_line_input:

  ; start of buffer
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

  rcall skip_whitespace

  ; return if buffer is empty
  ld r16, X
  or r16, r16
  brne PC+2
  ret

  ; parse the line number
  rcall parse_number

  ; overflow?
  brvc PC+3
  ldi r_error, error_number_out_of_range
  ret

  ; XXX if at end of buffer, delete this line

  ; parse the line back into the input buffer
  ldi YL, low(input_buffer)
  ldi YH, high(input_buffer)

  rcall parse_statement

  ; bail on parse error
  or r_error, r_error
  breq PC+2
  ret

  ; XXX skip remaining whitespace and look for end of buffer, error if not

  ; XXX dump the bytecode buffer
  ;ldi ZL, low(input_buffer)
  ;ldi ZH, high(input_buffer)
  ;ldi r16, 0x10
  ;ldi r17, 0x00
  ;rcall usart_tx_bytes_hex

  ; check if we have a line number
  brts find_instruction_location

  ; no line number, this is immediate mode and we can just execute it
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

  rcall execute_statement

  ret

find_instruction_location:

  ; Y currently pointing at end of bytecode. subtract start of buffer to find length
  mov r24, YL
  ldi r16, low(input_buffer)
  sub r24, r16

  ; setup pointer to first instruction
  ldi YL, low(program_buffer)
  ldi YH, high(program_buffer)

  ; T flag indicates whether this is the last instruction in the program. if
  ; so, we will set the empty end-of-program instruction after we store the new
  ; instruction. setting it for the most common case of adding the line to the
  ; end of the program
  set

consider_instruction:
  ; load the length
  ld r16, Y

  ; check for the empty end-of-program instruction
  ; if its here, we can go directly to append
  or r16, r16
  breq append_instruction

  ; advance past the length field
  adiw YL, 1

  ; load the line number
  ld r17, Y+
  ld r18, Y+

  ; compare the line number we're looking with the one we just parsed
  cp r2, r17
  cpc r3, r18

  ; if its the same number, we're replacing it
  breq replace_instruction

  ; if its lower than us, then we belong here and need to push forward
  brlo open_instruction

  ; its higher than us, so we need to move along and try the next one

  ; Y currently point at the oplist, which is #r16 long, so skip past it
  add YL, r16
  brcc consider_instruction
  inc YH
  rjmp consider_instruction

append_instruction:

  ; need to check if there's room. compute position of new top pointer
  mov XL, YL
  mov XH, YH
  adiw XL, 4 ; length + 2xlineno + end-of-program
  add XL, r16
  brcc PC+2
  inc XH

  ; see if we've gone past the end
  ldi r19, low(program_buffer_end)
  ldi r20, high(program_buffer_end)
  cp r19, XL
  cpc r20, XH
  brsh PC+3

  ; aww
  ldi r_error, error_out_of_memory
  ret

  rjmp store_instruction

replace_instruction:

  ldi r16, 'R'
  rcall usart_tx_byte
  rjmp blink_forever

open_instruction:

  ; make a gap of #r24 + 3 bytes here

  ; get pointer to top of memory
  mov XL, r_top_l
  mov XH, r_top_h

  ; add room for the instruction, making X our move target
  adiw XL, 3
  add XL, r24
  brcc PC+2
  inc XH

  ; see if we've gone past the end
  ldi r19, low(program_buffer_end)
  ldi r20, high(program_buffer_end)
  cp r19, XL
  cpc r20, XH
  brsh PC+3

  ; aww
  ldi r_error, error_out_of_memory
  ret

  ; move source is just the existing top
  mov ZL, r_top_l
  mov ZH, r_top_h

  ; new top is in X
  mov r_top_l, XL
  mov r_top_h, XH

  ; Y currently pointing at the oplist of the instruction we're inserting
  ; before. roll it back a bit
  subi YL, 3
  brcc PC+2
  dec YH

  ; now copy from Z -> X, rolling down until Z = Y
  ld r4, -Z
  st -X, r4
  cp ZL, YL
  cpc ZH, YH
  brne PC-4

  ; no end-of-program instruction
  clt

  ; fall through to store_instruction

store_instruction:

  ; Y at start of instruction, which has room

  ; store length
  st Y+, r24

  ; store line number
  st Y+, r2
  st Y+, r3

  ; copy #r24 bytes from op buffer
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  ld r16, X+
  st Y+, r16
  dec r24
  brne PC-3

  brts store_end_of_program
  ret

store_end_of_program:
  ; zero the length on the next instruction
  clr r16
  st Y+, r16

  ; moved top of memory
  mov r_top_l, YL
  mov r_top_h, YH

  ret




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


; parse a statment (keyword + args)
; inputs:
;   X: pointer to statement text, will be moved
parse_statement:

  rcall skip_whitespace

  ; take copy of pointer to start of statement, so we can reset it
  mov r4, XL
  mov r5, XH

  ; start of keyword table
  ldi ZL, low(keyword_table*2)
  ldi ZH, high(keyword_table*2)

  ; walk both strings, comparing as we go. if we fail a compare, reset X, jump
  ; Z forward to next string
keyword_loop:
  lpm r17, Z+

  ; if its below the ascii caps area, then its an opcode and we matched
  cpi r17, 0x40
  brlo keyword_end

  ; load the next char of the input keyword and compare
  ld r16, X+
  cp r16, r17
  breq keyword_loop

  ; chars didn't match, so we have to start over on the next keyword

  ; reset X to start of input keyword
  mov XL, r4
  mov XH, r5

  ; walk Z forward to the next keyword
  lpm r17, Z+
  cpi r17, 0x40
  brsh PC-2

  rjmp keyword_loop

keyword_end:

  ; but if its zero, we hit the end of the keyword table, so it wasn't found
  or r17, r17
  brne PC+3

  ldi r_error, error_no_such_keyword
  ret

  rcall skip_whitespace

  ; store the opcode
  st Y+, r17

  ; set up for rest of statement parse
  ldi ZL, low(keyword_parse_table)
  ldi ZH, high(keyword_parse_table)

  ; add opcode to get the parser vector
  add ZL, r17
  brcc PC+2
  inc ZH

  ; go there, return to caller
  ijmp


keyword_table:
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

keyword_parse_table:
  .dw 0             ; 0x00 [reserved]
  rjmp parse_print  ; 0x01 PRINT expr-list
  rjmp parse_if     ; 0x02 IF expression relop expression THEN statement
  rjmp parse_goto   ; 0x03 GOTO expression
  rjmp parse_input  ; 0x04 INPUT var-list
  rjmp parse_let    ; 0x05 LET var = expression
  rjmp parse_gosub  ; 0x06 GOSUB expression
  ret               ; 0x07 RETURN
  ret               ; 0x08 CLEAR
  ret               ; 0x09 LIST
  ret               ; 0x0a RUN
  ret               ; 0x0b END
  ret               ; 0x0c [ON]
  ret               ; 0x0d [OFF]
  ret               ; 0x0e [SLEEP]


parse_print:
  ret

parse_if:
  ret

parse_goto:
  push r2
  push r3

  ; XXX expression
  rcall parse_number

  ; overflow?
  brvc PC+3
  ldi r_error, error_number_out_of_range
  rjmp parse_goto_done

  ; was there even a number?
  brts PC+3
  ldi r_error, error_expected_number
  rjmp parse_goto_done

  ; store it
  st Y+, r2
  st Y+, r3

parse_goto_done:
  pop r3
  pop r2

  ret


  ret

parse_input:
  ret

parse_let:
  ret

parse_gosub:
  ret


execute_program:

  ; clear last error
  clr r_error

  ; set next line pointer to start of program buffer
  ldi r_next_l, low(program_buffer)
  ldi r_next_h, high(program_buffer)

execute_mainloop:

  ; setup to read line
  mov XL, r_next_l
  mov XH, r_next_h

  ; look for end-of-program marker (length 0)
  ld r16, X+
  or r16, r16
  breq execute_done

  ; advance next instruction pointer
  add r_next_l, r16 ; skip #r16 bytes of opbuffer
  ldi r16, 3
  adc r_next_l, r16 ; skip length+lineno
  brcc PC+2
  inc r_next_h

  ; skip the line number
  adiw XL, 2

  ; statement now at X, execute it
  rcall execute_statement

  ; error check
  or r_error, r_error
  breq execute_mainloop

  ; error
  rcall handle_error
  ; XXX IN LINE 30

  ; program done!
execute_done:
  ret

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
  ld r4, X+
  ld r5, X+

  ; get pointer to start of program buffer
  ldi r19, low(program_buffer)
  ldi r20, high(program_buffer)

op_goto_search_loop:

  ; setup to read line
  mov XL, r19
  mov XH, r20

  ; look for end-of-program marker (length 0)
  ld r16, X+
  or r16, r16
  breq op_goto_not_found

  ; load line number
  ld r17, X+
  ld r18, X+

  cp r4, r17
  brne PC+2
  cp r5, r18
  breq op_goto_found

  ; advance to next instruction
  add r19, r16 ; skip #r16 bytes of opbuffer
  ldi r16, 3
  adc r19, r16 ; skip length+lineno
  brcc op_goto_search_loop
  inc r20
  rjmp op_goto_search_loop

op_goto_found:

  ; found it, set the next line pointer for execution to here
  mov r_next_l, r19
  mov r_next_h, r20

  ; return from command; mainloop will continue at the line we set
  ret

op_goto_not_found:

  ; abort
  ldi r_error, error_no_such_line
  ret


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

  ; get pointer to start of program buffer
  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)

op_list_next:
  ld r16, X
  or r16, r16
  brne PC+2
  ret

  ldi r17, 3
  add r16, r17

  mov ZL, XL
  mov ZH, XH
  clr r17

  add XL, r16
  brcc PC+2
  inc XH

  rcall usart_tx_bytes_hex

  rjmp op_list_next


op_run:
  rjmp execute_program

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
  subi XL, 1
  brcc PC+2
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
  push r16
  swap r16,
  andi r16, 0x0f
  ldi r17, 0x30
  add r16, r17
  cpi r16, 0x3a
  brlo PC+3
  ldi r17, 0x27
  add r16, r17
  rcall usart_tx_byte

  pop r16
  andi r16, 0x0f
  ldi r17, 0x30
  add r16, r17
  cpi r16, 0x3a
  brlo PC+3
  ldi r17, 0x27
  add r16, r17
  rjmp usart_tx_byte


; transmit a hex representation of a block of data via the usart
; inputs:
;   Y: pointer to start of data in sram
;   r16: number of bytes to transmit
usart_tx_bytes_hex:
  mov r18, r16
  mov r19, r17
  ldi r20, 0x10

usart_tx_bytes_hex_start_line:
  mov r16, ZH
  rcall usart_tx_byte_hex
  mov r16, ZL
  rcall usart_tx_byte_hex
  ldi r16, ' '
  rcall usart_tx_byte
  rcall usart_tx_byte

usart_tx_bytes_hex_next:
  ld r16, Z+
  rcall usart_tx_byte_hex

  dec r18
  brne PC+4
  cpi r19, 0
  breq usart_tx_bytes_hex_done
  dec r19

  dec r20
  breq PC+4

  ldi r16, ' '
  rcall usart_tx_byte
  rjmp usart_tx_bytes_hex_next

  ldi r20, 0x10

  ldi r16, 0xa
  rcall usart_tx_byte
  ldi r16, 0xd
  rcall usart_tx_byte

  rjmp usart_tx_bytes_hex_start_line

usart_tx_bytes_hex_done:
  ldi r16, 0xa
  rcall usart_tx_byte
  ldi r16, 0xd
  rcall usart_tx_byte

  ret


text_newline:
  .db 0xa, 0xd, 0

text_banner:
  .db "GOOD COMPUTER", 0
text_prompt:
  .db "BASIC> ", 0

text_error_no_such_keyword:
  .db "NO SUCH KEYWORD", 0
text_error_number_out_of_range:
  .db "NUMBER OUT OF RANGE", 0
text_error_expected_number:
  .db "EXPECTED NUMBER", 0
text_error_no_such_line:
  .db "NO SUCH LINE", 0
text_error_out_of_memory:
  .db "OUT OF MEMORY", 0
