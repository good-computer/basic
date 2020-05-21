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

; expression stack 0360-037f
.equ expr_stack     = input_buffer - 0x20
.equ expr_stack_end = input_buffer - 1

; linked list of program instructions 0060->
.equ program_buffer     = SRAM_START
.equ program_buffer_end = expr_stack - 1

; global registers
.def r_error  = r21 ; last error code
.def r_next_l = r22 ; memory location of next instruction
.def r_next_h = r23
.def r_top_l  = r24 ; pointer to top of program (one past end-of-program marker)
.def r_top_h  = r25

; error codes
.equ error_no_such_keyword     = 1
.equ error_number_out_of_range = 2
.equ error_expected_number     = 3
.equ error_no_such_line        = 4
.equ error_out_of_memory       = 5
.equ error_mismatched_parens   = 6
.equ error_overflow            = 7


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
  movw r_top_l, XL


main:

  ; say hello
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print_static
  ldi ZL, low(text_banner*2)
  ldi ZH, high(text_banner*2)
  rcall usart_print_static
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print_static


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

  ; show the prompt
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rcall usart_print_static
  ldi ZL, low(text_prompt*2)
  ldi ZH, high(text_prompt*2)
  rcall usart_print_static

  ; read a line
  rcall usart_line_input

  ; and do stuff on it
  rcall handle_line_input

  ; error check
  or r_error, r_error
  breq main_loop

  ; show error text
  clt ; immediate
  rcall handle_error
  rjmp main_loop


; output error info
; inputs:
;   r_error: error code
;   T: if set, include "IN LINE XXXXX"
;   r16:r17: if T set, line number
handle_error:

  ; if T set, push r16:r17 for later
  brtc PC+3
  push r16
  push r17

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
  movw ZL, XL
  rcall usart_print_static

  ; do line number
  brtc error_newline

  ldi ZL, low(text_in_line*2)
  ldi ZH, high(text_in_line*2)
  rcall usart_print_static

  pop r17
  pop r16
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  rcall format_number

  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  rcall usart_print

error_newline:
  ; and the newline
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rjmp usart_print_static

error_lookup_table:
  .dw text_error_no_such_keyword*2
  .dw text_error_number_out_of_range*2
  .dw text_error_expected_number*2
  .dw text_error_no_such_line*2
  .dw text_error_out_of_memory*2
  .dw text_error_mismatched_parens*2
  .dw text_error_overflow*2


; move X forward until there's no whitespace under it
skip_whitespace:
  ld r16, X
  cpi r16, 0x20
  breq PC+2
  ret
  adiw XL, 1
  rjmp skip_whitespace


; consider the line buffer and do the right thing
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

  ; save T flag so we can test immediate mode later
  clr r15
  bld r15, 0

  ; parse the line back into the input buffer (as op buffer)
  ldi YL, low(input_buffer)
  ldi YH, high(input_buffer)

  rcall parse_statement

  ; XXX skip remaining whitespace and look for end of buffer, error if not

  ; bail on parse error
  or r_error, r_error
  breq PC+2
  ret

  ; if we have a line number, store it
  or r15, r15
  brne find_instruction_location

  ; no line number, this is immediate mode and we can just execute it
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

  rcall execute_statement

  ret

find_instruction_location:
  ; we have a line number, so we need to add it to the program

  ; XXX if at end of buffer, delete this line

  ; Y currently pointing at end of op we just parsed. subtract start of buffer
  ; to find length
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
  movw XL, YL
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
  movw XL, r_top_l

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
  movw ZL, r_top_l

  ; new top is in X
  movw r_top_l, XL

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
  movw r_top_l, YL

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
  movw r6, XL

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
  movw r4, r0

  ; multiply accumulator high byte by 10
  mul r3, r18
  add r5, r0
  brcc PC+2
  inc r1
  cp r1, r19
  brne parse_number_overflow

  ; actually read something, flag this
  set

  ; reload the accumulator after multiplying
  movw r2, r4

  ; convert digit to value, and add
  andi r17, 0xf
  add r2, r17
  brcc parse_number_loop
  inc r3
  brne parse_number_loop

  ; overflowed, restore, X flag and abort
parse_number_overflow:
  movw XL, r6
  sev
  ret


; parse a statment (keyword + args)
; inputs:
;   X: pointer to statement text, will be moved
parse_statement:

  rcall skip_whitespace

  ; take copy of pointer to start of statement, so we can reset it
  movw r4, XL

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
  movw XL, r4

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
      "RESET",  0xf, \
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
  ret               ; 0x0f [RESET]


parse_print:
  push r2
  push r3

  rcall parse_expression

  ; bail on parse error
  or r_error, r_error
  brne parse_print_done

  ; XXX expr-list

parse_print_done:
  pop r3
  pop r2

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


parse_input:
  ret

parse_let:
  ret

parse_gosub:
  ret


parse_expression:

  ; prep expression stack pointer at one behind the stack area
  ; (so pointer always points to top item on stack)
  ldi ZL, low(expr_stack-1)
  ldi ZH, high(expr_stack-1)

expr_next:
  rcall skip_whitespace

  ; next char
  ld r16, X

  ; operands (numbers, variables) go to the output buffer, with suitable
  ; micro-ops so we know how to resolve them at runtime

  cpi r16, 0x30
  brlo expr_maybe_var
  cpi r16, 0x3a
  brsh expr_maybe_var

  ; a number, try to parse it
  rcall parse_number

  ; overflow?
  brvc PC+3
  ldi r_error, error_number_out_of_range
  ret

  ; literal number marker
  ldi r16, 0x1
  st Y+, r16

  ; the number
  st Y+, r2
  st Y+, r3

  rjmp expr_next

expr_maybe_var:

  ; XXX check for var, send to output buffer

expr_maybe_oper:

  ; inital check that its an operator; this is doubling up a little but makes
  ; future checks easier
  ; operators are ( ) * + - / (28-2b,2d,2f)
  cpi r16, 0x28
  brlo expr_dump_remaining_opers
  cpi r16, 0x2c
  brlo expr_start_oper_stack
  cpi r16, 0x2d
  breq expr_start_oper_stack
  cpi r16, 0x2f
  breq expr_start_oper_stack

  ; nothing interesting, so end of expression
  ; pop remaining operators and send to output

expr_dump_remaining_opers:

  ; check if stack is empty
  cpi ZL, low(expr_stack)
  brsh PC+4

  ; done! add terminator and get out of here
  clr r16
  st Y+, r16
  ret

  ; pop
  ld r17, Z
  dec ZL

  ; check for parens, shouldn't be here
  cpi r17, '('
  brne PC+3

  ; ohnoes
  ldi r_error, error_mismatched_parens
  ret

  ; send to output
  st Y+, r17

  rjmp expr_dump_remaining_opers

expr_start_oper_stack:
  ; take the operator
  adiw XL, 1

  ; right paren first, since it must never go on stack
  cpi r16, ')'
  brne expr_try_oper

  ; closing paren, so pop all the operators to the opening paren and add them to the op buffer
expr_take_opers:

  ; top of stack check
  cpi ZL, low(expr_stack)
  brsh PC+3

  ; ran out of stack before we found the opening paren
  ldi r_error, error_mismatched_parens
  ret

  ; take the top item
  ld r16, Z
  dec ZL

  ; left paren terminates
  cpi r16, '('
  breq PC+3

  ; anything else, push to output, go for next
  st Y+, r16
  rjmp expr_take_opers

  rjmp expr_next

expr_try_oper:
  ; check if stack empty
  cpi ZL, low(expr_stack)
  brlo expr_push_oper

  ; see what's on the stack
  ld r17, Z
  cpi r17, '('
  brne expr_left_paren

expr_push_oper:
  ; stack empty or left paren on stack, so push the oper
  inc ZL
  st Z, r16
  rjmp expr_next

expr_left_paren:

  ; left paren goes straight to the stack
  cpi r16, '('
  brne expr_oper_precedence

  inc ZL
  st Z, r16
  rjmp expr_next

expr_oper_precedence:

  ; - if higher precendence, push onto stack
  ; - if equal precedence, pop and output, then push
  ; - if lower precedence, pop and output, then retest

  ; inspect top of stack
  ld r17, Z

  ; everything is higher precedence than left paren
  cpi r17, '('
  breq expr_oper_higher_precedence

  ; only * and / can have higher precedence
  cpi r16, '*'
  breq PC+3
  cpi r16, '/'
  brne expr_oper_check_plusminus_precedence

  ; check equal precedence
  cpi r17, '*'
  breq expr_oper_equal_precedence
  cpi r17, '/'
  breq expr_oper_equal_precedence

expr_oper_higher_precedence:
  ; higher precedence, push
  inc ZL
  st Z, r16
  rjmp expr_next

expr_oper_check_plusminus_precedence:

  ; its a plus or a minus, check stack for equal
  cpi r17, '+'
  breq expr_oper_equal_precedence
  cpi r17, '-'
  breq expr_oper_equal_precedence

  ; lower precedence then stack. pop and output, then retest
  dec ZL
  st Y+, r17

  rjmp expr_try_oper

expr_oper_equal_precedence:

  ; equal precedence, pop and output, then push
  ld r17, -Z
  st Y+, r17

  inc ZL
  st Z, r17
  rjmp expr_next





execute_program:

  ; clear last error
  clr r_error

  ; set next line pointer to start of program buffer
  ldi r_next_l, low(program_buffer)
  ldi r_next_h, high(program_buffer)

execute_mainloop:

  ; setup to read line
  movw XL, r_next_l

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

  ; push line number in case we have to report an error
  ld r16, X+
  push r16
  ld r16, X+
  push r16

  ; statement now at X, execute it
  rcall execute_statement

  ; pop line number back for error report
  pop r17
  pop r16

  ; error check
  or r_error, r_error
  breq execute_mainloop

  ; error
  set ; include line number
  rcall handle_error

  ; program done!
execute_done:
  ret


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
  rjmp op_reset   ; 0x0f [RESET]

op_print:
  rcall eval_expression

  or r_error, r_error
  breq PC+2
  ret

  push XL
  push XH
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  st X+, r16
  st X+, r17
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  rcall format_number
  pop XH
  pop XL

  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  rcall usart_print

  ret

op_if:
  ret

op_goto:
  ; target line
  ld r4, X+
  ld r5, X+

  ; get pointer to start of program buffer
  ldi r18, low(program_buffer)
  ldi r19, high(program_buffer)

op_goto_search_loop:

  ; setup to read line
  movw XL, r18

  ; look for end-of-program marker (length 0)
  ld r20, X+
  or r20, r20
  breq op_goto_not_found

  ; load line number
  ld r16, X+
  ld r17, X+

  cp r4, r16
  brne PC+2
  cp r5, r17
  breq op_goto_found

  ; advance to next instruction
  add r18, r20 ; skip #r20 bytes of opbuffer
  ldi r20, 3
  adc r18, r20 ; skip length+lineno
  brcc op_goto_search_loop
  inc r19
  rjmp op_goto_search_loop

op_goto_found:

  ; found it, set the next line pointer for execution to here
  movw r_next_l, r18

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

  ; XXX this is very stupid, just hexdumps each line

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

  movw ZL, XL
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

op_reset:

  ; enable watchdog timer to force reset in ~16ms
  cli
  ldi r16, (1<<WDE)
  out WDTCR, r16
  rjmp PC


; evaluate expression
; inputs:
;   X: expression op
; outputs:
;   r16:r17: result
eval_expression:

  ; use the input buffer as the eval stack
  ldi YL, low(input_buffer)
  ldi YH, high(input_buffer)

eval_next:
  ; get expr micro-op
  ld r16, X+

  ; terminator
  cpi r16, 0x0
  brne eval_check_literal

  ; pop result!
  ld r17, -Y
  ld r16, -Y

  ret

eval_check_literal:

  ; numeric literal
  cpi r16, 0x1
  brne eval_check_add

  ; push number onto stack
  ld r16, X+
  ld r17, X+
  st Y+, r16
  st Y+, r17

  rjmp eval_next

eval_check_add:

  ; add
  cpi r16, '+'
  brne eval_check_sub

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A + B
  add r16, r18
  adc r17, r19
  brcc PC+3

  ldi r_error, error_overflow
  ret

  ; push result
  st Y+, r16
  st Y+, r17

  rjmp eval_next

eval_check_sub:

  ; sub
  cpi r16, '-'
  brne eval_check_mul

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A - B
  sub r16, r18
  sbc r17, r19
  brcc PC+3

  ldi r_error, error_overflow
  ret

  ; push result
  st Y+, r16
  st Y+, r17

  rjmp eval_next

eval_check_mul:

  ; mul
  cpi r16, '*'
  brne eval_check_div

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A * B

  ; this is a full 16x16->32 bit multiply, even though we're limited to 16-bit
  ; math for the moment. this is actually slightly easier (for me) to write and
  ; we're going to need it in the future anyway.
  clr r20
  mul r17, r19
  movw r4, r0
  mul r16, r18
  movw r2, r0
  mul r17, r18
  add r3, r0
  adc r4, r1
  adc r5, r20
  mul r16, r19
  add r3, r0
  adc r4, r1
  adc r5, r20

  ; check overflow
  or r4, r5
  breq PC+3

  ldi r_error, error_overflow
  ret

  ; push result
  st Y+, r2
  st Y+, r3

  rjmp eval_next

eval_check_div:

  ; div
  cpi r16, '/'
  breq PC+2

  ; XXX can't happen? found something on the stack we weren't expecting
  rjmp blink_forever

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; taken from AVR200 app note
  clr r4        ; clear remainder Low byte
  sub r5, r5    ; clear remainder High byte and carry
  ldi r20, 17   ; init loop counter
div_loop:
  rol r16       ; shift left dividend
  rol r17
  dec r20       ; decrement counter
  breq div_done ; if done
  rol r4        ; shift dividend into remainder
  rol r5
  sub r4, r18   ; remainder = remainder - divisor
  sbc r5, r19
  brcc PC+5     ; if result negative
  add r4, r18   ;     restore remainder
  adc r5, r19
  clc           ; clear carry to be shifted into result
  rjmp div_loop ; else
  sec           ; set carry to be shifted into result
  rjmp div_loop

div_done:

  ; push result
  st Y+, r16
  st Y+, r17

  rjmp eval_next


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


; take a number, convert to ASCII
; inputs:
;   r16:r17: number
; outputs
;   X: ascii
format_number:
  ldi ZL, low(decades*2)
  ldi ZH, high(decades*2)

  ; accumulator, to handle zero padding
  clr r19

format_number_loop:
  ldi r18, 0x2f ; just before ASCII '0'

  ; get decade (10 multiplier)
  lpm r2, Z+
  lpm r3, Z+

  ; repeatedly subtract until we go negative
  inc r18
  sub r16, r2
  sbc r17, r3
  brsh PC-3

  ; add back the remainder
  add r16, r2
  adc r17, r3

  ; accumulate bottom bits of result; while its zero, we're in leading zeros
  ; and shouldn't emit anything
  add r19, r18
  andi r19, 0xf
  breq PC+2

  ; emit a digit!
  st X+, r18

  ; move to next decade
  cpi ZL, low(decades*2)+5*2
  brne format_number_loop

  ; trailing zero
  clr r16
  st X+, r16

  ret

decades:
  .dw 10000, 1000, 100, 10, 1


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
usart_print_static:
  lpm r16, Z+
  cpi r16, 0
  breq PC+3
  rcall usart_tx_byte
  rjmp PC-4
  ret

; transmit a null-terminated string via the usart
; inputs:
;   Z: pointer to start of string in sram
usart_print:
  ld r16, Z+
  cpi r16, 0
  breq PC+3
  rcall usart_tx_byte
  rjmp PC-4
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
  rjmp usart_tx_byte

  ; that's all the input!

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
  ; high nybble
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

  ; low nybble
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
  movw r18, r16
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

text_in_line:
  .db " IN LINE ", 0

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
text_error_mismatched_parens:
  .db "MISMATCHED PARENS", 0
text_error_overflow:
  .db "OVERFLOW", 0
