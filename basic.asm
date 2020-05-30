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

; op buffer 0360-037f->
; allowed to run into the input buffer if necessary, which almost certainly
; will be ahead during parse
.equ op_buffer = input_buffer - 0x20

; expression stack 0340-035f
.equ expr_stack     = op_buffer - 0x20
.equ expr_stack_end = op_buffer - 1

; gosub stack 0330-033f
.equ gosub_stack     = expr_stack - 0x10
.equ gosub_stack_end = expr_stack - 1

; list of variables <-0x32f (reverse direction)
.equ variable_buffer     = gosub_stack - 1
.equ variable_buffer_end = SRAM_START

; list of program instructions 0060->
.equ program_buffer     = SRAM_START
.equ program_buffer_end = gosub_stack - 1


; global registers
.def r_error    = r25 ; last error code
.def r_flags    = r10 ; global state flags
.def r_gosub_sp = r11 ; low byte of top of gosub stack
.def r_next_l   = r12 ; memory location of next instruction
.def r_next_h   = r13
.def r_top_l    = r14 ; pointer to top of program (one past end-of-program marker)
.def r_top_h    = r15

; error codes
.equ error_number_out_of_range  = 1
.equ error_expected_number      = 2
.equ error_expected_variable    = 3
.equ error_expected_statement   = 4
.equ error_expected_comparator  = 5
.equ error_expected_expression  = 6
.equ error_expected_operand     = 7
.equ error_expected_string      = 8
.equ error_mismatched_parens    = 9
.equ error_unterminated_string  = 10
.equ error_return_without_gosub = 11
.equ error_if_without_then      = 12
.equ error_out_of_memory        = 13
.equ error_overflow             = 14
.equ error_no_such_line         = 15
.equ error_type_mismatch        = 16


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


  ; setup
  rcall op_new


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
  tst r_error
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

  ldi ZL, low(text_at_line*2)
  ldi ZH, high(text_at_line*2)
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
  .dw text_error_number_out_of_range*2
  .dw text_error_expected_number*2
  .dw text_error_expected_variable*2
  .dw text_error_expected_statement*2
  .dw text_error_expected_comparator*2
  .dw text_error_expected_expression*2
  .dw text_error_expected_operand*2
  .dw text_error_expected_string*2
  .dw text_error_mismatched_parens*2
  .dw text_error_unterminated_string*2
  .dw text_error_return_without_gosub*2
  .dw text_error_if_without_then*2
  .dw text_error_out_of_memory*2
  .dw text_error_overflow*2
  .dw text_error_no_such_line*2
  .dw text_error_type_mismatch*2


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
  tst r16
  brne PC+2
  ret

  ; parse the line number
  rcall parse_number
  tst r_error
  breq PC+2
  ret

  ; negative?
  tst r3
  brpl PC+3

  ; no negative numbers for line numbers sorrt
  ldi r_error, error_number_out_of_range
  ret

  ; copy line number so we can safely trample it in statement parsing
  movw r8, r2

  ; save T flag so we can test immediate mode later
  clr r_flags
  bld r_flags, 0

  ; parse the line back into the immediate buffer
  ldi YL, low(op_buffer)
  ldi YH, high(op_buffer)

  rcall skip_whitespace

  ; end of buffer check. if its empty, then it has to be because we got a line
  ; number and nothing else, otherwise we'd hit the trap above. proceed
  ; immediately to "store"
  ld r16, X
  tst r16
  breq find_instruction_location

  ; stuff! parse it
  rcall parse_statement_list

  ; XXX skip remaining whitespace and look for end of buffer, error if not

  ; bail on parse error
  tst r_error
  breq PC+2
  ret

  brts PC+3
  ldi r_error, error_expected_statement
  ret

  ; if we have a line number, store it
  tst r_flags
  brne find_instruction_location

  ; no line number, this is immediate mode and we can just execute it
  ldi XL, low(op_buffer)
  ldi XH, high(op_buffer)

  rjmp execute_statement

find_instruction_location:
  ; we have a line number, so we need to add it to the program

  rcall op_clear

  ; Y currently pointing at end of op we just parsed. subtract start of buffer
  ; to find length
  mov r24, YL
  ldi r16, low(op_buffer)
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
  tst r16
  breq append_instruction

  ; advance past the length field
  adiw YL, 1

  ; load the line number
  ld r17, Y+
  ld r18, Y+

  ; move Y back to start of instruction
  sbiw YL, 3

  ; compare the line number we're looking with the one we just parsed
  cp r8, r17
  cpc r9, r18

  ; if its the same number, we're replacing it
  breq replace_instruction

  ; if its lower than us, then we belong here and need to push forward
  brlo insert_instruction

  ; its higher than us, so we need to move along and try the next one

  ; move Y to next slot position, #r16 long (+3)
  adiw YL, 3
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

insert_instruction:

  ; want room for the whole lot, with room for housekeeping
  mov r23, r24
  ldi r22, 3
  add r23, r22
  rjmp open_instruction_slot

replace_instruction:

  ; #r16 has current length. #r24 has what we need. how much do we need to grow/shrink by?
  mov r23, r24
  sub r23, r16

  ; if its same size, go straight to store
  breq store_instruction

  ; grow? just a smaller open
  brsh open_instruction_slot

  ; shrink
  rjmp close_instruction_slot

open_instruction_slot:

  ; make a gap of #r23 bytes here

  ; get pointer to top of memory
  movw XL, r_top_l

  ; add room for the instruction, making X our move target
  add XL, r23
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

  ; now copy from Z -> X, rolling down until Z = Y
  ld r4, -Z
  st -X, r4
  cp ZL, YL
  cpc ZH, YH
  brne PC-4

  ; no end-of-program instruction
  clt

  rjmp store_instruction

close_instruction_slot:

  ; pull back -#r23 bytes here

  ; if we're replacing it with nothing, then need to pull back 3 more
  ; housekeeping bytes
  tst r24
  brne PC+3

  ; sub offset
  ldi r22, 3
  sub r23, r22

  ; get target into X
  movw XL, YL

  ; and source, -#r23 ahead
  movw ZL, YL
  sub ZL, r23 ; sub, because its negative and we want to go forward
  brcs PC+2
  dec ZH

  ; now copy from Z -> X, rolling up until Z = r_top
  ld r4, Z+
  st X+, r4
  cp ZL, r_top_l
  cpc ZH, r_top_h
  brne PC-4

  ; new top is in X
  movw r_top_l, XL

  ; just closed a gap and nothing to replace it with was just deleting a line,
  ; and we're done
  tst r24
  brne PC+2
  ret

  ; no end-of-program instruction
  clt

  ; fall through to store instruction

store_instruction:

  ; Y at start of instruction, which has room

  ; store length
  st Y+, r24

  ; store line number
  st Y+, r8
  st Y+, r9

  ; copy #r24 bytes from op buffer
  ldi XL, low(op_buffer)
  ldi XH, high(op_buffer)
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
parse_number:

  ; store X in case we need to rewind
  movw r6, XL

  ; clear result flag
  clt

  ; accumulator
  clr r2
  clr r3

  ldi r18, 10 ; for multiplying by 10 repeatedly
  clr r19

  ; check first char for negation
  ld r20, X
  cpi r20, '-'
  brne parse_number_loop
  ; skip it, leave it in r20 to test at the end
  adiw XL, 1

parse_number_loop:
  ; get input char and check range
  ld r17, X
  cpi r17, 0x30
  brlo PC+3
  cpi r17, 0x3a
  brlo digit_valid

  ; out of range, this is the end

  ; did we read anything?
  brts PC+3

  ; no, just roll X back and abort
  movw XL, r6
  ret

  ; see if we need to negate
  cpi r20, '-'
  brne PC+5

  ; do so!
  com r3
  neg r2
  ldi r17, 0xff
  sbc r3, r17

  ret

digit_valid:
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

  ; take char
  adiw XL, 1

  ; reload the accumulator after multiplying
  movw r2, r4

  ; convert digit to value, and add
  andi r17, 0xf
  add r2, r17
  brcc parse_number_loop
  inc r3
  brpl parse_number_loop

parse_number_overflow:
  ; overflowed, abort
  ldi r_error, error_number_out_of_range
  ret


parse_statement_list:

  ; count number of statements parse
  clr r16
  push r16

statement_next:
  ; try to parse one
  rcall parse_statement
  tst r_error
  breq PC+3
  pop r16
  ret

  ; did we get one?
  brts statement_got

  ; no, set T if we got any at all
statement_last:
  clt
  pop r16
  tst r16
  breq PC+2
  set
  ret

statement_got:
  ; got one, bump the count
  pop r16
  inc r16
  push r16

  rcall skip_whitespace

  ; following byte
  ld r16, X

  ; end of input, leave it there and get out of here
  tst r16
  brne PC+3

  ; store the zero as an end-of-statement marker
  st Y+, r16
  rjmp statement_last

  ; statement separator
  cpi r16, ':'
  breq PC+4

  ; something weird, bail
  pop r16
  ldi r_error, error_expected_statement
  ret

  ; take it
  adiw XL, 1

  ; store it
  st Y+, r16

  ; go again
  rcall skip_whitespace
  rjmp statement_next


; parse a statement (keyword + args)
; inputs:
;   X: pointer to statement text, will be moved
; outputs:
;   Y: oplist, will be moved
;   T:  set if we actually parsed something
parse_statement:

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
  tst r17
  brne PC+4

  ; reset X
  movw XL, r4

  ; flag nothing parsed
  clt
  ret

  ; parsed stuff, tell the caller
  set

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
  .db "PRINT",  0x01, \
      "IF",     0x02, \
      "GOTO",   0x03, \
      "INPUT",  0x04, \
      "LET",    0x05, \
      "GOSUB",  0x06, \
      "RETURN", 0x07, \
      "NEW",    0x08, \
      "CLEAR",  0x09, \
      "LIST",   0x0a, \
      "RUN",    0x0b, \
      "END",    0x0c, \
                      \
      "ON",     0x0d, \
      "OFF",    0x0e, \
      "SLEEP",  0x0f, \
      "RESET",  0x10, \
                      \
      0

keyword_parse_table:
  .dw 0                ; 0x00 [reserved]
  rjmp st_parse_print  ; 0x01 PRINT expr-list
  rjmp st_parse_if     ; 0x02 IF expression relop expression THEN statement
  rjmp st_parse_goto   ; 0x03 GOTO expression
  rjmp st_parse_input  ; 0x04 INPUT var-list
  rjmp st_parse_let    ; 0x05 LET var = expression
  rjmp st_parse_goto   ; 0x06 GOSUB expression
  ret                  ; 0x07 RETURN
  ret                  ; 0x08 NEW
  ret                  ; 0x09 CLEAR
  ret                  ; 0x0a LIST
  ret                  ; 0x0b RUN
  ret                  ; 0x0c END
  ret                  ; 0x0d [ON]
  ret                  ; 0x0e [OFF]
  ret                  ; 0x0f [SLEEP]
  ret                  ; 0x10 [RESET]


st_parse_print:

  rcall skip_whitespace

  ; check for end of input, no expression is valid
  ld r16, X

  tst r16
  breq st_parse_print_done

  ; tread unadorned colon as end of input as well. need this because we can't
  ; rely on their being an explicit separator between expressions, so we can't
  ; reliably know when to expect end-of-statenent in the face of a valid
  ; (weird) statement like 'PRINT "A" "B"'
  cpi r16, ':'
  brne st_parse_print_try

st_parse_print_done:
  ; nothing to parse, record null and eject
  clr r16
  st Y+, r16

  ; parsed stuff
  set
  ret

st_parse_print_try:

  rcall parse_expression
  tst r_error
  breq PC+2
  ret
  brts st_parse_print_sep

  ; who knows
  ldi r_error, error_expected_expression
  ret

st_parse_print_sep:

  rcall skip_whitespace

  ; look for separators
  ld r16, X

  cpi r16, ';'
  breq PC+4
  cpi r16, ','
  breq PC+2

  ; no separator, loop for next expression/string/nothing
  rjmp st_parse_print

  ; found one, take it
  adiw XL, 1

  ; store it
  st Y+, r16

  ; and go again
  rjmp st_parse_print


st_parse_if:

  rcall skip_whitespace

  rcall parse_expression
  tst r_error
  breq PC+2
  ret

  brts PC+3
  ldi r_error, error_expected_expression
  ret

  rcall skip_whitespace

  ; =  0x01
  ; <  0x02
  ; <= 0x04
  ; <> 0x08
  ; >  0x10
  ; >= 0x20

  ld r16, X+
  cpi r16, '='
  breq comparator_equal
  cpi r16, '<'
  breq comparator_left
  cpi r16, '>'
  breq comparator_right

comparator_unknown:
  ldi r_error, error_expected_comparator
  ret

comparator_left:
  ld r16, X+
  cpi r16, '='
  brne PC+3
  ldi r16, 0x04
  rjmp comparator_store
  cpi r16, '>'
  brne PC+3
  ldi r16, 0x08
  rjmp comparator_store
  sbiw XL, 1
  ldi r16, 0x02
  rjmp comparator_store

comparator_right:
  ld r16, X+
  cpi r16, '='
  brne PC+3
  ldi r16, 0x20
  rjmp comparator_store
  sbiw XL, 1
  ldi r16, 0x10
  rjmp comparator_store

comparator_equal:
  ldi r16, 0x01
comparator_store:
  st Y+, r16

  rcall skip_whitespace

  rcall parse_expression
  tst r_error
  breq PC+2
  ret

  brts PC+3
  ldi r_error, error_expected_expression
  ret

  rcall skip_whitespace

  ; check for THEN, sigh
  ldi ZL, low(text_then*2)
  ldi ZH, high(text_then*2)
  lpm r16, Z+
  tst r16
  breq PC+6
  ld r17, X+
  cp r16, r17
  breq PC-5

  ldi r_error, error_if_without_then
  ret

  rcall skip_whitespace

  rcall parse_statement_list
  tst r_error
  breq PC+2
  ret
  brts PC+2
  ldi r_error, error_expected_statement
  ret

text_then:
  .db "THEN", 0


st_parse_goto:
  rcall parse_expression

  tst r_error
  breq PC+2
  ret

  brts PC+2
  ldi r_error, error_expected_expression

  tst r16
  breq PC+2
  ldi r_error, error_type_mismatch

  ret


st_parse_input:

  ; find a variable name
  rcall parse_var
  tst r_error
  breq PC+2
  ret
  brts PC+3

  ldi r_error, error_expected_variable
  ret

  ; store its name
  st Y+, r16

  ; comma?
  ld r16, X
  cpi r16, ','
  brne PC+3

  ; advance and try for another
  adiw XL, 1
  rjmp st_parse_input

  ; end of variables marker
  clr r16
  st Y+, r16

  ret


st_parse_let:

  ; find a variable name
  rcall parse_var
  tst r_error
  breq PC+2
  ret
  brts PC+3

  ldi r_error, error_expected_variable
  ret

  st Y+, r16
  push r16

  rcall skip_whitespace

  ; skip the =
  ld r16, X
  cpi r16, '='
  brne PC+2
  adiw XL, 1

  rcall skip_whitespace

  pop r16
  andi r16, 0x80
  breq st_parse_let_expr

  rcall parse_string
  tst r_error
  breq PC+2
  ret

  brts PC+2
  ldi r_error, error_expected_string

  ret

st_parse_let_expr:
  rcall parse_expression
  tst r_error
  breq PC+2
  ret

  brts PC+2
  ldi r_error, error_expected_expression

  ret


; parse a double-quoted string
; inputs:
;   X: pointer to double-quoted string
; outputs:
;   Y: double-quote marker, then null-terminated string
;   T: set if we actually parsed something
parse_string:

  ; confirm its a string
  ld r16, X
  cpi r16, '"'
  breq PC+3

  ; bounce out
  clt
  ret

  ; get this started
  set

  ; take the double-quote
  ld r16, X+
  st Y+, r16

string_loop:

  ; take everything up to and the closing double-quote
  ld r16, X+

  ; hit end of input early?
  tst r16
  brne PC+3

  ldi r_error, error_unterminated_string
  ret

  ; ending double-quote?
  cpi r16, '"'
  breq PC+3

  st Y+, r16
  rjmp string_loop

  ; add null terminator
  clr r16
  st Y+, r16

  ret


; parse an expression into the opbuffer
; sets r16 if expression is stringery, clears if numbery
parse_expression:

  ; prep expression stack pointer at one behind the stack area
  ; (so pointer always points to top item on stack)
  ldi ZL, low(expr_stack-1)
  ldi ZH, high(expr_stack-1)

  ; r21 tracks parse state
  ; bit 0: expecting operand [0] or operator [1]
  ; bit 1: numeric expression
  ; bit 2: string expression

  ; start by accepting numeric and string, until we see the first operand and
  ; disable the other type
  ldi r21, 0x6

expr_next:
  rcall skip_whitespace

  ; next char
  ld r16, X

  ; check expectations
  sbrc r21, 0
  rjmp expr_check_operator

  ; expecting an operand. value types, and opening paren allowed

  ; operands (numbers, strings, variables) go to the output buffer, with
  ; suitable micro-ops so we know how to resolve them at runtime

  ; try to parse a number
  rcall parse_number
  tst r_error
  breq PC+2
  ret

  ; did we even get a number?
  brtc expr_maybe_string

  ; are we accepting numbers?
  bst r21, 1
  brts PC+3

  ; nope
  ldi r_error, error_type_mismatch
  ret

  ; no longer accepting strings
  cbr r21, 0x4

  ; literal number marker
  ldi r16, 0x1
  st Y+, r16

  ; the number
  st Y+, r2
  st Y+, r3

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_maybe_string:

  ; try to parse a string
  rcall parse_string
  tst r_error
  breq PC+2
  ret

  ; did we even get a string?
  brtc expr_maybe_var

  ; are we accepting strings?
  bst r21, 2
  brts PC+3

  ; nope
  ldi r_error, error_type_mismatch
  ret

  ; no longer accepting numbers
  cbr r21, 0x2

  ; already pushed to Y by parse_string

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_maybe_var:

  ; what about a variable?
  rcall parse_var
  tst r_error
  breq PC+2
  ret

  ; maybe!
  brtc expr_maybe_left_paren

  ; make sure it matches the type we want
  ; r16 bit 7 is true for string var, false for numeric
  tst r16
  brmi PC+4

  ; a string, load string bit, clear number bit
  bst r21, 1
  cbr r21, 0x4
  rjmp PC+3

  ; a number, load number bit, clear string bit
  bst r21, 2
  cbr r21, 0x2

  ; continue if its the right type for the var
  brts PC+3

  ; nope
  ldi r_error, error_type_mismatch
  ret

  ; variable lookup!
  ldi r17, 0x2
  st Y+, r17

  ; var name
  st Y+, r16

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_maybe_left_paren:

  ; left paren goes straight to the stack
  cpi r16, '('
  brne expr_not_operand

  ; take it
  adiw XL, 1

  ; stack it
  inc ZL
  st Z, r16

  ; want operand again, so no change to r21
  rjmp expr_next

expr_not_operand:

  ; needed an operand, but didn't get one

  ; if this was the first operand, that's ok, just means that this isn't an
  ; expression really. X is already back at the start
  cpi ZL, low(expr_stack)
  brsh PC+3

  ; stack empty, so we can just leave
  clt
  ret

  ; sorry, really needed that thing
  ldi r_error, error_expected_operand
  ret

expr_check_operator:

  ; operator permission check

  ; ) and + work for all operand types
  cpi r16, ')'
  breq expr_start_oper_stack
  cpi r16, '+'
  breq expr_start_oper_stack

  ; - * / only work for numbers
  cpi r16, '-'
  breq expr_check_number_oper
  cpi r16, '*'
  breq expr_check_number_oper
  cpi r16, '/'
  breq expr_check_number_oper

  ; nothing interesting, so end of expression

expr_dump_remaining_opers:
; pop remaining operators and send to output

  ; check if stack is empty
  cpi ZL, low(expr_stack)
  brsh PC+7

  ; done! add terminator and get out of here
  clr r16
  st Y+, r16

  ; set r16 if expression is stringy
  bst r21, 2
  bld r16, 0

  ; did parse things!
  set
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

expr_check_number_oper:
  bst r21, 1
  brts expr_start_oper_stack

  ldi r_error, error_type_mismatch
  ret

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
  brne expr_oper_precedence

expr_push_oper:
  ; stack empty or left paren on stack, so push the oper
  inc ZL
  st Z, r16

  ; operand next
  cbr r21, 0x1
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

  ; operand next
  cbr r21, 0x1
  rjmp expr_next

expr_oper_check_plusminus_precedence:

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

  ; output the one on the stack
  st Y+, r17

  ; "push" the new one. just replace top of stack
  st Z, r16

  ; operand next
  cbr r21, 0x1
  rjmp expr_next


; parse a var name
; inputs:
;   X: pointer to variable name, will be moved
; outputs:
;   r16: variable name
;   T:  set if we actually parsed something
parse_var:
  clt

  ld r16, X
  cpi r16, 'A'
  brsh PC+2
  ret

  cpi r16, 'Z'+1
  brlo PC+2
  ret

  ; found it, take it
  adiw XL, 1

  ; flag that we got something
  set

  ; look for $ (string varname)
  ld r17, X
  cpi r17, '$'
  breq PC+2
  ret

  ; found it, take it
  adiw XL, 1

  ; set top bit
  ori r16, 0x80

  ret


execute_program:

  ; clear last error
  clr r_error

  ; set next line pointer to start of program buffer
  ldi r16, low(program_buffer)
  ldi r17, high(program_buffer)
  movw r_next_l, r16

execute_mainloop:

  ; if next instruction is null, exit
  tst r_next_l
  brne PC+3
  tst r_next_h
  breq execute_done

  ; setup to read line
  movw XL, r_next_l

  ; look for end-of-program marker (length 0)
  ld r16, X+
  tst r16
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
  tst r_error
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
  add ZL, r16
  brcc PC+2
  inc ZH

  ; and jump to the handler
  icall

  ; error check and abort
  tst r_error
  breq PC+2
  ret

  ; look for statement terminator
  ld r16, X+

  ; null, we're done
  tst r16
  brne PC+2
  ret

  ; anything else (':'), there's another statement!
  rjmp execute_statement


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
  rjmp op_new     ; 0x08 NEW
  rjmp op_clear   ; 0x09 CLEAR
  rjmp op_list    ; 0x0a LIST
  rjmp op_run     ; 0x0b RUN
  rjmp op_end     ; 0x0c END
  rjmp op_on      ; 0x0d [ON]
  rjmp op_off     ; 0x0e [OFF]
  rjmp op_sleep   ; 0x0f [SLEEP]
  rjmp op_reset   ; 0x10 [RESET]

op_print:

  ; T flag says to add a newline at the end
  set

print_next:

  ; what's next?
  ld r16, X+

  ; nothing to print? newline only thanks
  tst r16
  breq PC+3

  ; colon also means nothing, same dumb reasonsing as in st_parse_print
  cpi r16, ':'
  brne print_sep

  ; do we want a newline?
  brts PC+2
  ret

  ; yep
  ldi ZL, low(text_newline*2)
  ldi ZH, high(text_newline*2)
  rjmp usart_print_static

print_sep:
  ; comma?
  cpi r16, ','
  brne PC+4

  ; emit a tab
  ldi r16, 0x9
  rcall usart_tx_byte
  rjmp print_next

  ; semicolon?
  cpi r16, ';'
  brne PC+3

  ; disable newline
  clt
  rjmp print_next

print_expr:

  ; back up to expression start
  sbiw XL, 1

  rcall eval_expression

  tst r_error
  breq PC+2
  ret

  brtc print_number

  ; null pointer means valid string expression but vars not found, don't print
  tst r16
  brne PC+3
  tst r17
  breq PC+3

  ; print string at returned pointer
  movw ZL, r16
  rcall usart_print

  ; want new line again
  set
  rjmp print_next

print_number:
  ; format number to start of input buffer
  push XL
  push XH
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  rcall format_number
  pop XH
  pop XL

  ; and print it
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  rcall usart_print

  ; newlines please!
  set
  rjmp print_next



op_if:

  ; first expression
  rcall eval_expression
  tst r_error
  breq PC+2
  ret

  ; set them aside
  push r16
  push r17

  ; operator
  ld r16, X+
  push r16

  ; second expression
  rcall eval_expression

  ; get first expr and operator back
  pop r20
  pop r19
  pop r18

  ; second eval error check
  tst r_error
  breq PC+2
  ret

  ; DEBUG dump operands and comparators
;  movw r6, r16
;
;  mov r16, r18
;  rcall usart_tx_byte_hex
;  mov r16, r19
;  rcall usart_tx_byte_hex
;
;  ldi r16, ' '
;  rcall usart_tx_byte
;
;  mov r16, r20
;  rcall usart_tx_byte_hex
;
;  ldi r16, ' '
;  rcall usart_tx_byte
;
;  mov r16, r6
;  rcall usart_tx_byte_hex
;  mov r16, r7
;  rcall usart_tx_byte_hex
;
;  ldi r16, ' '
;  rcall usart_tx_byte
;
;  movw r16, r6
;
;  ldi r16, 0x41
;  add r16, r6
;  rcall usart_tx_byte
;  ldi r16, 0x41
;  add r16, r7
;  rcall usart_tx_byte
;  ldi r16, 0x41
;  add r16, r18
;  rcall usart_tx_byte
;  ldi r16, 0x41
;  add r16, r19
;  rcall usart_tx_byte
;
;  ldi r16, ' '
;  rcall usart_tx_byte
;
;  movw r16, r6
  ; END DEBUG

  ; now do r18:r19 [r20 compop] r16:r17
  cp r18, r16
  cpc r19, r17
  breq comp_eq
  brlt comp_lt

  ; =  0x01
  ; <  0x02
  ; <= 0x04
  ; <> 0x08
  ; >  0x10
  ; >= 0x20

comp_gt:
  ; greater than: matching ops 0x08 <> | 0x10 > | 0x20 >=
  andi r20, 0x08|0x10|0x20
  brne comp_match
  ret

comp_eq:
  ; equal: matching ops 0x01 = | 0x04 <= | 0x20 >=
  andi r20, 0x01|0x04|0x20
  brne comp_match
  ret

comp_lt:
  ; less than: matching ops 0x02 < | 0x04 <= | 0x08 <>
  andi r20, 0x02|0x04|0x08
  brne comp_match
  ret

comp_match:
  ; matched! now we can execute the statement
  rjmp execute_statement


op_goto:

  rcall eval_expression
  tst r_error
  breq PC+2
  ret

  ; target line
  movw r4, r16

  ; get pointer to start of program buffer
  ldi r18, low(program_buffer)
  ldi r19, high(program_buffer)

op_goto_search_loop:

  ; setup to read line
  movw XL, r18

  ; look for end-of-program marker (length 0)
  ld r20, X+
  tst r20
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

  ; get the var name
  ld r16, X+

  ; done all vars?
  tst r16
  brne PC+2
  ret

  push r16
  push XL
  push XH

  ldi ZL, low(text_input_prompt*2)
  ldi ZH, high(text_input_prompt*2)
  rcall usart_print_static

  ; read a line
  rcall usart_line_input

  ; start of buffer
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)

  rcall skip_whitespace

  ; return if buffer is empty (variable will retain its value)
  ld r16, X
  tst r16
  brne PC+5
  pop XH
  pop XL
  pop r16
  rjmp op_input

  ; parse the value
  rcall parse_number

  pop XH
  pop XL
  pop r16

  ; number parse error?
  tst r_error
  breq PC+2
  ret

  ; did we get anything?
  brts PC+3

  ldi r_error, error_expected_number
  ret

  ; reset input buffer for number store
  ldi YL, low(input_buffer)
  ldi YH, high(input_buffer)
  movw ZL, YL

  ; buffer the number
  st Y+, r2
  st Y+, r3

  ; name in r16, so we can set
  ldi r17, 0x2

  push XH
  push XL
  rcall set_variable
  pop XL
  pop XH

  ; try next
  rjmp op_input


op_let:

  ; get variable name
  ld r16, X+
  push r16

  ; determine type
  bst r16, 7
  brtc op_let_expr

  ; skip double-quote
  adiw XL, 1

  ; scan and count length
  clr r17
  mov YL, XL
  inc r17
  ld r16, Y+
  tst r16
  brne PC-3

  ; get varname back
  pop r16

  ; start of buffer
  movw ZL, XL

  rcall set_variable

  ; point X past end of string
  movw XL, ZL

  ret

op_let_expr:
  ; do the math
  rcall eval_expression

  ; setup a storage buffer for the value
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)

  ; push number
  sts input_buffer,   r16
  sts input_buffer+1, r17

  pop r16
  ldi r17, 0x2

  push XL
  push XH

  rcall set_variable

  pop XH
  pop XL

  ret


op_gosub:

  ; make sure there's room on the gosub stack
  mov ZL, r_gosub_sp
  cpi ZL, low(gosub_stack_end+1)
  brne PC+3

  ldi r_error, error_out_of_memory
  ret

  ; stack the next line pointer
  ldi ZH, high(gosub_stack)
  st Z+, r_next_l
  st Z+, r_next_h

  ; do a normal goto, which will advance r_next_l
  rcall op_goto

  ; see if goto failed
  tst r_error
  brne PC+3

  ; success, advance the stack pointer
  ldi r16, 2
  add r_gosub_sp, r16

  ret


op_return:

  ; make sure there's something on the gosub stack
  mov ZL, r_gosub_sp
  cpi ZL, low(gosub_stack)
  brne PC+3

  ldi r_error, error_return_without_gosub
  ret

  ; pop the next line pointer
  ldi ZH, high(gosub_stack);
  ld r_next_h, -Z
  ld r_next_l, -Z

  ; save the new stack pointer back
  mov r_gosub_sp, ZL

  ret


op_new:
  ; make first instruction zero-length, truncating the entire program
  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)
  clr r16
  st X, r16

  ; set top pointer just past the zero-length instruction
  movw r_top_l, XL

  ; clear next instruction
  mov r_next_l, r16
  mov r_next_h, r16

  ; fall through


op_clear:
  ; clear gosub stack
  ldi r16, low(gosub_stack)
  mov r_gosub_sp, r16

  ; clear variable space
  clr r16
  sts variable_buffer, r16

  ret


op_list:

  ; XXX this is very stupid, just hexdumps each line

  ; get pointer to start of program buffer
  ldi XL, low(program_buffer)
  ldi XH, high(program_buffer)

op_list_next:
  ld r16, X
  tst r16
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
  rcall op_clear

  rjmp execute_program


op_end:

  ; clear next instruction
  clr r_next_l
  clr r_next_h
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
;   T: result type, string [1] or number [0]
eval_expression:

  ; use the input buffer as the eval stack
  ldi YL, low(input_buffer)
  ldi YH, high(input_buffer)

  ; r21 tracks eval state
  ; bit 1: numeric expression --- set on first operand
  ; bit 2: string expression  -/
  clr r21

eval_next:

  ; get expr micro-op
  ld r16, X+

  ; terminator
  tst r16
  brne eval_check_number

  ; pop result!
  ld r17, -Y
  ld r16, -Y

  ; indicate result type to caller
  bst r21, 2

  ret

eval_check_number:

  ; number
  cpi r16, 0x1
  brne eval_check_string

  ; numeric expression
  sbr r21, 0x2

  ; push number onto stack
  ld r16, X+
  ld r17, X+
  st Y+, r16
  st Y+, r17

  rjmp eval_next

eval_check_string:

  ; string
  cpi r16, '"'
  brne eval_check_var

  ; string expression
  sbr r21, 0x4

  ; push pointer to string on stack
  ;adiw XL, 1
  st Y+, XL
  st Y+, XH

  ; walk X forward to end of string
  ld r16, X+
  tst r16
  brne PC-2

  rjmp eval_next

eval_check_var:

  ; variable lookup
  cpi r16, 0x2
  brne eval_check_add

  ; wanted name
  ld r16, X+

  ; set number/string mode
  bst r16, 7
  brts PC+3
  sbr r21, 0x2
  rjmp PC+2
  sbr r21, 0x4

  ; setup pointer to first variable
  ldi ZL, low(variable_buffer)
  ldi ZH, high(variable_buffer)

eval_try_var:
  ; load the length
  ld r18, Z

  ; look for end-of-variable marker
  tst r18
  brne PC+5

  ; unknown vars yield a 0
  clr r16
  st Y+, r16
  st Y+, r16

  rjmp eval_next

  ; get name
  ld r17, -Z

  ; compare
  cp r16, r17
  breq eval_found_var

  ; not this one, try next. take the name
  sbiw ZL, 1

  ; skip #r18 bytes of value
  sub ZL, r18
  brcc eval_try_var
  dec ZH
  rjmp eval_try_var

eval_found_var:

  bld r21, 1
  brtc eval_push_numeric_var

  ; string expression
  sbr r21, 0x4

  ; pull Z back to start of string (#r18 bytes back)
  sub ZL, r18
  brcc PC+2
  dec ZH

  ; push pointer to string in variable
  st Y+, ZL
  st Y+, ZH

  rjmp eval_next

eval_push_numeric_var:

  ; numeric expression
  sbr r21, 0x2

  ; push its value
  ld r16, -Z
  ld r17, -Z
  st Y+, r17
  st Y+, r16

  rjmp eval_next

eval_check_add:

  ; add
  cpi r16, '+'
  brne eval_check_sub

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A + B
  add r16, r18
  adc r17, r19
  brvc PC+3

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

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A - B
  sub r16, r18
  sbc r17, r19
  brvc PC+3

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

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; A * B

  ; taken from app note AVR200 (mul16s)
  clr  r3         ; clear result byte 3
  sub  r2, r2     ; clear result byte 2 and carry
  ldi  r20, 16    ; init loop counter
mul_loop:
  brcc PC+3       ; if carry (previous bit) set
  add  r2, r16    ;   add multiplicand Low to result byte 2
  adc  r3, r17    ;   add multiplicand High to result byte 3
  sbrc r18, 0     ; if current bit set
  sub  r2, r16    ;   sub multiplicand Low from result byte 2
  sbrc r18, 0     ; if current bit set
  sbc  r3, r17    ;   sub multiplicand High from result byte 3
  asr  r3         ; shift right result and multiplier
  ror  r2
  ror  r19
  ror  r18
  dec  r20        ; decrement counter
  brne mul_loop   ; if not done, loop more

  ; check overflow. top two bytes should be sign extension and that's all
  and r2, r3
  tst r19
  brpl PC+2
  inc r2
  tst r2
  breq PC+3

  ldi r_error, error_overflow
  ret

  ; push result
  st Y+, r18
  st Y+, r19

  rjmp eval_next

eval_check_div:

  ; div
  cpi r16, '/'
  breq PC+3

  ; XXX can't happen? found something on the stack we weren't expecting
  rcall usart_tx_byte
  rjmp blink_forever

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; taken from app note AVR200 (div16s)
  mov  r4, r17        ; move dividend High to sign register
  eor  r4, r19        ; xor divisor High with sign register
  sbrs r17, 7         ; if MSB in dividend set
  rjmp PC+5
  com  r17            ;    change sign of dividend
  com  r16
  subi r16, 0xff
  sbci r16, 0xff
  sbrs r19, 7         ; if MSB in divisor set
  rjmp PC+4
  com  r19            ;    change sign of divisor
  neg  r18
  sbci r19, 0xff
  clr  r2             ; clear remainder Low byte
  sub  r3, r3         ; clear remainder High byte and carry
  ldi  r20, 17        ; init loop counter
div_loop:
  rol  r16            ; shift left dividend
  rol  r17
  dec  r20            ; decrement counter
  brne PC+8           ; if done
  sbrs r4, 7          ;    if MSB in sign register set
  rjmp PC+4
  com  r17            ;        change sign of result
  neg  r16
  sbci r17, 0xff
  rjmp div_done       ;    return
  rol  r2             ; shift dividend into remainder
  rol  r3
  sub  r2, r18        ; remainder = remainder - divisor
  sbc  r3, r19        ;
  brcc PC+5           ; if result negative
  add  r2, r18        ;    restore remainder
  adc  r3, r19
  clc                 ;    clear carry to be shifted into result
  rjmp div_loop       ; else
  sec                 ;    set carry to be shifted into result
  rjmp div_loop

div_done:

  ; push result
  st Y+, r16
  st Y+, r17

  rjmp eval_next


; store a value to a named variable slot
; inputs:
;   r16: name of variable
;   r17: length of value data
;   Z: value data
set_variable:

  ; setup pointer to first variable
  ldi YL, low(variable_buffer)
  ldi YH, high(variable_buffer)

consider_variable:
  ; load the length
  ld r19, Y

  ; look for end-of-variable marker
  tst r19
  breq append_variable

  ; load the name
  ld r18, -Y

  ; compare
  cp r18, r16

  ; if its the same name, we're replacing it
  breq replace_variable

  ; not this one, need to try next. take the name
  sbiw YL, 1

  ; skip #r19 bytes of value
  sub YL, r19
  brcc consider_variable
  dec YH
  rjmp consider_variable

append_variable:

  ; calculate new end pointer, make sure we have room
  movw XL, YL
  sbiw XL, 1
  sub XL, r17
  brcc PC+2
  dec XH

  ; see if we've gone past the end
  ldi r19, low(variable_buffer_end+1)
  ldi r20, high(variable_buffer_end+1)
  cp r19, XL
  cpc r20, XH
  brlo PC+3

  ; aww
  ldi r_error, error_out_of_memory
  ret

  ; last variable
  set

  rjmp store_variable

replace_variable:

  ; replacing this slot. check if lengths match
  cp r17, r19
  brne adjust_variable_space

  ; same space, so back Y up to the length, then store here
  adiw YL, 1

  ; reusing the space
  clt

  rjmp store_variable

adjust_variable_space:

  ldi r16, 'r'
  rcall usart_tx_byte
  rjmp blink_forever

store_variable:

  ; Y at start of variable space

  ; store length
  st Y, r17

  ; store name
  st -Y, r16

  ; move to start of value storage
  sub YL, r17
  brcc PC+2
  dec YH

  ; save position, since we're about to drive forward
  push YL
  push YH

  ; copy #r17 bytes from Z -> Y
  ld r16, Z+
  st Y+, r16
  dec r17
  brne PC-3

  ; return position and advance
  pop YH
  pop YL

  brts store_end_of_variables
  ret

store_end_of_variables:

  ; store end marker
  clr r16
  st -Y, r16

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

  ; test negative
  tst r17
  brpl format_number_loop

  ; negate
  com r17
  neg r16
  sbci r17, 0xff

  ; emit leading minus sign
  ldi r18, '-'
  st X+, r18

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
  cpi ZL, low((decades+5)*2)
  brne format_number_loop

  ; if we didn't emit anything, then it was zero
  tst r19
  brne PC+3

  ; ascii zero
  ldi r18, 0x30
  st X+, r18

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
  tst r16
  breq PC+3
  rcall usart_tx_byte
  rjmp PC-4
  ret

; transmit a null-terminated string via the usart
; inputs:
;   Z: pointer to start of string in sram
usart_print:
  ld r16, Z+
  tst r16
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
;   Z: pointer to start of data in sram
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
  tst r19
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
text_input_prompt:
  .db "INPUT? ", 0

text_at_line:
  .db " AT LINE ", 0

text_error_number_out_of_range:
  .db "NUMBER OUT OF RANGE", 0
text_error_expected_number:
  .db "EXPECTED NUMBER", 0
text_error_expected_variable:
  .db "EXPECTED VARIABLE", 0
text_error_expected_statement:
  .db "EXPECTED STATEMENT", 0
text_error_expected_comparator:
  .db "EXPECTED COMPARATOR", 0
text_error_expected_expression:
  .db "EXPECTED EXPRESSION", 0
text_error_expected_operand:
  .db "EXPECTED OPERAND", 0
text_error_expected_string:
  .db "EXPECTED STRING", 0
text_error_mismatched_parens:
  .db "MISMATCHED PARENS", 0
text_error_unterminated_string:
  .db "UNTERMINATED STRING", 0
text_error_return_without_gosub:
  .db "RETURN WITHOUT GOSUB", 0
text_error_if_without_then:
  .db "IF WITHOUT THEN", 0
text_error_out_of_memory:
  .db "OUT OF MEMORY", 0
text_error_overflow:
  .db "OVERFLOW", 0
text_error_no_such_line:
  .db "NO SUCH LINE", 0
text_error_type_mismatch:
  .db "TYPE MISMATCH", 0
