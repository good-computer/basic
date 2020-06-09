; vim: ft=avr

;.device ATmega8
.include "m8def.inc"

; XXX I wonder if there's a better way to set up a memory map

; input buffer
.equ input_buffer     = 0x0060
.equ input_buffer_end = 0x00df

; gosub stack
.equ gosub_stack     = 0x00e0
.equ gosub_stack_end = 0x00ff

; location in internal memory for current linemap page (high byte)
.equ linemap_buffer_h = high(0x100)

; variable storage (reverse direction)
.equ variable_buffer     = 0x037f
.equ variable_buffer_end = 0x0200

; op buffer
.equ op_buffer = 0x0380

; string buffer (temp space for strings being created)
.equ string_buffer     = 0x03b0
.equ string_buffer_end = 0x03df

; expression stack
.equ expr_stack     = 0x03e0
.equ expr_stack_end = 0x03ef

; external ram tracking
.equ r_opmem_top_l   = 0x03fc ; top of opmem (position of next instruction)
.equ r_opmem_top_h   = 0x03fd

; rng state
.equ rand_l = 0x03fe
.equ rand_h = 0x03ff

; stack top
.equ stack_top = 0x045f






.equ opmem_base   = 0x0000
.equ opmem_top    = 0xefff

; location in external memory of linemap
; map of line number (2 bytes) -> pointer to op buffer (2 bytes)
; only high byte gets considered, so must be on an even page boundary
.equ linemap_base = 0xf000
.equ linemap_top  = 0x0000



; global registers
.def r_error    = r25 ; last error code
.def r_flags    = r24 ; global state flags
.def r_gosub_sp = r15 ; low byte of top of gosub stack


; flag bits
.equ f_immediate  = 0
.equ f_abort_line = 1
.equ f_jump       = 2
.equ f_end        = 3

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
.equ error_for_without_to       = 13
.equ error_out_of_memory        = 14
.equ error_overflow             = 15
.equ error_no_such_line         = 16
.equ error_type_mismatch        = 17
.equ error_division_by_zero     = 18
.equ error_invalid_immediate    = 19
.equ error_invalid_program      = 20
.equ error_break                = 21

; expression ops
.equ expr_op_number   = 1
.equ expr_op_string   = 2
.equ expr_op_variable = 3
.equ expr_op_add      = 4
.equ expr_op_subtract = 5
.equ expr_op_multiply = 6
.equ expr_op_divide   = 7
.equ expr_op_abs      = 8
.equ expr_op_rnd      = 9
.equ expr_op_LAST     = 10


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

  ; XXX DEBUG clear memory
  ldi XL, low(SRAM_START)
  ldi XH, high(SRAM_START)
  ldi r16, low(RAMEND)
  ldi r17, high(RAMEND)
  ldi r18, 0x55
  ldi r19, 0xaa
  st X+, r18
  st X+, r19
  cp XL, r16
  cpc XH, r17
  brlo PC-4

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

  ; output: PB0 = error LED, PB1 = user LED
  ;         PB2 = SPI /SS (SRAM /CS), PB3 = SPI MOSI, PB5 = SPI SCK
  ; input: PB4 = SPI MISO
  ; don't care: PB7
  ldi r16, (1<<PB0) | (1<<PB1) | (1<<PB2) | (1<<PB3) | (1<<PB5)
  out DDRB, r16
  ; drive SPI /SS high to disable it
  ldi r16, (1<<PB2)
  out PORTB, r16

  ; enable SPI, master mode, clock rate fck/4 (4MHz)
  ldi r16, (1<<SPE) | (1<<MSTR)
  ; enable SPI, master mode, clock rate fck/128
  out SPCR, r16
  ; SPI clock double (8MHz)
  ldi r16, (1<<SPI2X)
  out SPSR, r16

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
  .dw text_error_for_without_to*2
  .dw text_error_out_of_memory*2
  .dw text_error_overflow*2
  .dw text_error_no_such_line*2
  .dw text_error_type_mismatch*2
  .dw text_error_division_by_zero*2
  .dw text_error_invalid_immediate*2
  .dw text_error_invalid_program*2
  .dw text_error_break*2


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
  bld r_flags, f_immediate

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
  sbrc r_flags, f_immediate
  rjmp find_instruction_location

  ; no line number, this is immediate mode and we can just execute it
  ldi XL, low(op_buffer)
  ldi XH, high(op_buffer)

  rjmp execute_statement

find_instruction_location:
  ; we have a line number, so we need to add it to the program

  rcall op_clear

  ; Y currently pointing at end of op we just parsed. subtract start of buffer
  ; to find length
  mov r23, YL
  ldi r16, low(op_buffer)
  sub r23, r16

  ; set to write opbuffer out to opmem
  lds r16, r_opmem_top_l
  lds r17, r_opmem_top_h
  movw XL, r16 ; save for further down
  clr r18
  rcall ram_write_start

  ; write length
  mov r16, r23
  rcall ram_write_byte

  ; write buffer
  ldi ZL, low(op_buffer)
  ldi ZH, high(op_buffer)
  rcall ram_write_bytes

  rcall ram_end

  ; now we need to update the linemap with this line

  ; T flag indicates whether this is the last instruction in the program. if
  ; so, we will set the empty end-of-program instruction after we store the new
  ; instruction. setting it for the most common case of adding the line to the
  ; end of the program
  set

  ; setup ref to current linemap page (high byte)
  ldi r20, high(linemap_base)

  ; load the first page of the linemap
  clr r16
  mov r17, r20
  ldi r17, high(linemap_base)
  clr r18
  rcall ram_read_start

linemap_load:
  clr ZL
  ldi ZH, linemap_buffer_h
  clr r16
  rcall ram_read_bytes

  ; holding ram active, so we can easily load in the next page

  ; reset to start of buffer
  clr ZL
  ldi ZH, linemap_buffer_h

  ; walk the linemap, looking for the right place to put this

linemap_next:
  ; load the stored line number
  ld r16, Z+
  ld r17, Z+

  ; if we've found line zero, then this is where we put it
  tst r16
  brne PC+3
  tst r17
  breq append_line

  ; compare the line number we're looking with the one we just parsed
  cp r8, r16
  cpc r9, r17

  ; if its the same number, we're replacing it
  breq replace_line

  ; if its lower than us, then we belong here and need to push forward
  brlo insert_line

  ; its higher than us, so we need to move along and try the next one

  ; advancing; skip the opmem pointer
  adiw ZL, 2

  ; see if we've gone off the end of the page
  tst ZL
  brne linemap_next

  ; yep, load the next page
  inc r20
  rjmp linemap_load

insert_line:

  ; XXX open a gap, put this line in it

  sbi PORTB, PB0
  rjmp PC

replace_line:

  ; XXX consider length and remove line if we're replacing with nothing

  ; not changing the end marker, otherwise its the same as append
  clt

append_line:

  ; finish read
  rcall ram_end

  ; if we're at the last entry of the last linemap page, then we can't add any more
  cpi r20, high(linemap_top-1)
  brne PC+5
  cpi ZL, 0xfe ; Z is two bytes into last entry
  brne PC+3

  ldi r_error, error_out_of_memory
  ret

  ; move Z back to line number position
  sbiw ZL, 2

  ; prep for ram write; low byte is from Z, high byte from current page
  mov r16, ZL
  mov r17, r20
  clr r18
  rcall ram_write_start

  ; write line number
  movw r16, r8
  rcall ram_write_pair

  ; write opmem pointer
  movw r16, XL
  rcall ram_write_pair

  ; advance opmem top pointer past the #r23+1 bytes of opmem
  adiw XL, 1
  add XL, r23
  brcc PC+2
  inc XH

  sts r_opmem_top_l, XL
  sts r_opmem_top_h, XH

  ; if T is set, we just appended, and need to update the end marker
  brts PC+2
  rjmp ram_end

  ; write end-of-linemap marker
  clr r16
  clr r17
  rcall ram_write_pair

  ; write done!
  rjmp ram_end


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
  ; start of keyword table
  ldi ZL, low(keyword_table*2)
  ldi ZH, high(keyword_table*2)

  ; try to parse one
  rcall parse_token
  brts statement_got

statement_last:
  ; no, set T if we got any at all
  clt
  pop r16
  tst r16
  breq PC+2
  set
  ret

statement_got:
  ; got the keyword, subparse
  rcall skip_whitespace

  ; store the opcode
  st Y+, r17

  ; set up for rest of statement parse
  ldi ZL, low(keyword_subparser_table)
  ldi ZH, high(keyword_subparser_table)

  ; add opcode to get the parser vector
  add ZL, r17
  brcc PC+2
  inc ZH

  ; call keyword subparser
  icall

  ; error check
  tst r_error
  breq PC+3
  pop r16
  ret

  ; bump the count
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

keyword_table:
  .db "PRINT",  0x01, \
      "IF",     0x02, \
      "GOTO",   0x03, \
      "INPUT",  0x04, \
      "LET",    0x05, \
      "GOSUB",  0x06, \
      "RETURN", 0x07, \
      "FOR",    0x08, \
      "NEXT",   0x09, \
      "NEW",    0x0a, \
      "CLEAR",  0x0b, \
      "LIST",   0x0c, \
      "RUN",    0x0d, \
      "END",    0x0e, \
                      \
      "ON",     0x0f, \
      "OFF",    0x10, \
      "SLEEP",  0x11, \
      "RESET",  0x12, \
      "CLS",    0x13, \
                      \
      0

keyword_subparser_table:
  .dw 0                  ; 0x00 [reserved]
  rjmp st_parse_print    ; 0x01 PRINT expr-list
  rjmp st_parse_if       ; 0x02 IF expression relop expression THEN statement
  rjmp st_parse_goto     ; 0x03 GOTO expression
  rjmp st_parse_input    ; 0x04 INPUT var-list
  rjmp st_parse_let      ; 0x05 LET var = expression
  rjmp st_parse_goto     ; 0x06 GOSUB expression
  rjmp invalid_immediate ; 0x07 RETURN
  rjmp st_parse_for      ; 0x08 FOR var = expression TO expression
  rjmp st_parse_next     ; 0x09 NEXT var
  rjmp invalid_program   ; 0x0a NEW
  rjmp invalid_program   ; 0x0b CLEAR
  ret                    ; 0x0c LIST
  rjmp invalid_program   ; 0x0d RUN
  rjmp invalid_immediate ; 0x0e END
  ret                    ; 0x0f [ON]
  ret                    ; 0x10 [OFF]
  ret                    ; 0x11 [SLEEP]
  ret                    ; 0x12 [RESET]
  ret                    ; 0x13 [CLS]


invalid_immediate:
  sbrs r_flags, f_immediate
  ldi r_error, error_invalid_immediate
  ret

invalid_program:
  sbrc r_flags, f_immediate
  ldi r_error, error_invalid_program
  ret


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

  push r16

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
  pop r16
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

  pop r17

  tst r_error
  breq PC+2
  ret

  brts PC+3
  ldi r_error, error_expected_expression
  ret

  cp r16, r17
  breq PC+3
  ldi r_error, error_type_mismatch
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

  rcall invalid_immediate
  tst r_error
  breq PC+2
  ret

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

  ; set aside var for type check later
  push r16

  rcall skip_whitespace

  ; skip the =
  ld r16, X
  cpi r16, '='
  brne PC+2
  adiw XL, 1

  rcall skip_whitespace

  rcall parse_expression
  pop r17

  tst r_error
  breq PC+2
  ret

  brts PC+2
  ldi r_error, error_expected_expression

  ; consider type. var bit 7 and expr parse return bit 0s
  ; must be both set (string) or both clear (number)
  andi r17, 0x80
  or r17, r16
  tst r17
  breq PC+4
  cpi r17, 0x81
  breq PC+2

  ldi r_error, error_type_mismatch

  ret


st_parse_for:

  ; starts off like LET
  rcall st_parse_let
  tst r_error
  breq PC+2
  ret

  ; r16 still set with last call to parse_expression in LET, so can check numberness
  tst r16
  breq PC+3

  ldi r_error, error_type_mismatch
  ret

  rcall skip_whitespace

  ; TO sigh
  ld r16, X+
  cpi r16, 'T'
  brne PC+4
  ld r16, X+
  cpi r16, 'O'
  breq PC+3

  ldi r_error, error_for_without_to
  ret

  rcall skip_whitespace

  ; target expression
  rcall parse_expression
  tst r_error
  breq PC+2
  ret

  brts PC+3
  ldi r_error, error_expected_expression
  ret

  ; has to be numbery too
  tst r16
  breq PC+2
  ldi r_error, error_type_mismatch

  ret


st_parse_next:

  ; find a variable name
  rcall parse_var
  tst r_error
  breq PC+2
  ret
  brts PC+3

  ldi r_error, error_expected_variable
  ret

  ; require number
  tst r16
  brpl PC+3

  ldi r_error, error_type_mismatch
  ret

  ; store its name
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

  ; closing paren triggers stack take
  cpi r16, ')'
  brne PC+3
  adiw XL, 1
  rjmp expr_take_opers

  ; check expectations
  sbrc r21, 0
  rjmp expr_check_operator
  rjmp expr_check_operand

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

  ; check top bit, looking for function end
  tst r16
  brmi PC+3

  ; anything else, push to output, go for next
  st Y+, r16
  rjmp expr_take_opers

  ; drop the flag bit
  cbr r16, 0x80

  ; if its now zero, its a normal left paren and we're done
  breq PC+2

  ; otherwise push the op
  st Y+, r16

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_check_operand:
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
  ldi r16, expr_op_number
  st Y+, r16

  ; the number
  st Y+, r2
  st Y+, r3

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_maybe_string:

  ; try to parse a string
  ld r16, X
  cpi r16, '"'
  brne expr_maybe_function

  ; are we accepting strings?
  bst r21, 2
  brts PC+3

  ; nope
  ldi r_error, error_type_mismatch
  ret

  ; no longer accepting numbers
  cbr r21, 0x2

  ; take the double-quote
  adiw XL, 1

  ; push "string literal" expr op
  ldi r16, expr_op_string
  st Y+, r16

expr_string_loop:
  ; take everything up to and the closing double-quote
  ld r16, X+

  ; ran off the end
  tst r16
  brne PC+3
  ldi r_error, error_unterminated_string
  ret

  ; ending double-quote?
  cpi r16, '"'
  breq PC+3

  st Y+, r16
  rjmp expr_string_loop

  ; add null terminator
  clr r16
  st Y+, r16

  ; operator next
  sbr r21, 0x1

  rjmp expr_next

expr_maybe_function:

  ; start of function table
  push ZL
  push ZH

  ldi ZL, low(function_table*2)
  ldi ZH, high(function_table*2)

  ; try to parse one
  rcall parse_token

  pop ZH
  pop ZL

  brtc expr_maybe_var

  ;  stack the (marked) function opcode
  inc ZL
  st Z, r17

  ; still want operand, so no change

  rjmp expr_next

expr_maybe_var:

  ; what about a variable?
  rcall parse_var
  tst r_error
  breq PC+2
  ret

  ; maybe!
  brtc expr_not_operand

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
  ldi r17, expr_op_variable
  st Y+, r17

  ; var name
  st Y+, r16

  ; operator next
  sbr r21, 0x1

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

  ; do operator mode checks, remapping to expr opcodes as we go

  ; + works for all operand types
  cpi r16, '+'
  brne PC+3
  ldi r16, expr_op_add
  rjmp expr_start_oper

  ; - * / only work for numbers

  cpi r16, '-'
  brne PC+3
  ldi r16, expr_op_subtract
  rjmp expr_check_number_oper
  cpi r16, '*'
  brne PC+3
  ldi r16, expr_op_multiply
  rjmp expr_check_number_oper
  cpi r16, '/'
  brne PC+3
  ldi r16, expr_op_divide
  rjmp expr_check_number_oper

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
  tst r17
  brpl PC+3

  ; ohnoes
  ldi r_error, error_mismatched_parens
  ret

  ; send to output
  st Y+, r17

  rjmp expr_dump_remaining_opers

expr_check_number_oper:
  bst r21, 1
  brts expr_start_oper

  ldi r_error, error_type_mismatch
  ret

expr_start_oper:
  ; take the operator
  adiw XL, 1

expr_try_oper:
  ; check if stack empty
  cpi ZL, low(expr_stack)
  brlo expr_push_oper

  ; see what's on the stack
  ld r17, Z
  tst r17
  brpl expr_oper_precedence

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
  tst r17
  brmi expr_oper_higher_precedence

  ; only * and / can have higher precedence
  cpi r16, expr_op_multiply
  breq PC+3
  cpi r16, expr_op_divide
  brne expr_oper_check_plusminus_precedence

  ; check equal precedence
  cpi r17, expr_op_multiply
  breq expr_oper_equal_precedence
  cpi r17, expr_op_divide
  breq expr_oper_equal_precedence

expr_oper_higher_precedence:
  ; higher precedence, push
  inc ZL
  st Z, r16

  ; operand next
  cbr r21, 0x1
  rjmp expr_next

expr_oper_check_plusminus_precedence:

  cpi r17, expr_op_add
  breq expr_oper_equal_precedence
  cpi r17, expr_op_add
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

function_table:
  .db "(",    0x80,             \
      "ABS(", 0x80|expr_op_abs, \
      "RND(", 0x80|expr_op_rnd, \
      0


; parse a token
; inputs:
;   X: pointer to input text, will be moved
;   Z: pointer to start of token table, will be moved
; outputs:
;   r17: opcode
;   T:  set if we actually parsed something
parse_token:

  ; take copy of pointer to start of statement, so we can reset it
  movw r4, XL

  ; walk both strings, comparing as we go. if we fail a compare, reset X, jump
  ; Z forward to next string
token_loop:
  lpm r17, Z+

  ; if its outside the ascii printables area, then its an opcode and we matched
  cpi r17, 0x20
  brlo token_end
  cpi r17, 0x7f
  brsh token_end

  ; load the next char of the input token and compare
  ld r16, X+
  cp r16, r17
  breq token_loop

  ; chars didn't match, so we have to start over on the next token

  ; reset X to start of input token
  movw XL, r4

  ; walk Z forward to the next token
  lpm r17, Z+
  cpi r17, 0x20
  brlo token_loop
  cpi r17, 0x7f
  brlo PC-4

  rjmp token_loop

token_end:

  ; but if its zero, we hit the end of the token table, so it wasn't found
  tst r17
  brne PC+4

  ; reset X
  movw XL, r4

  ; flag nothing parsed
  clt
  ret

  ; parsed stuff, tell the caller
  set
  ret


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

  ; set top bit to mark string type
  sbr r16, 0x80

  ret


execute_program:

  ; clear last error
  clr r_error

  ; start at start of linemap
  clr r20
  ldi r21, high(linemap_base)

exec_linemap_load:

  ; load page #r21 of the linemap
  clr r16
  mov r17, r21
  clr r18
  rcall ram_read_start

  clr ZL
  ldi ZH, linemap_buffer_h
  clr r16
  rcall ram_read_bytes
  rcall ram_end

  ; move to wanted position from start or jump
  mov ZL, r20
  ldi ZH, linemap_buffer_h

  ; walk the linemap, working out what to run next

exec_linemap_next:
  ; load the line number, just to see if we hit the end of program
  ld r18, Z+
  ld r19, Z+

  ; if we reached the end, we're done
  tst r18
  brne PC+4
  tst r19
  brne PC+2

  ; fell off the end, program done
  ret

  ; load opmem pointer
  ld r16, Z+
  ld r17, Z+

  ; save linemap position
  push ZL
  push r21

  ; copy linemap position for op (eg gosub)
  mov r20, ZL

  ; save line number, for error reports
  push r18
  push r19

  ; set op buffer for read
  ldi ZL, low(op_buffer)
  ldi ZH, high(op_buffer)

  ; set up for read
  clr r18
  rcall ram_read_start

  ; read the length
  rcall ram_read_byte

  ; and fill the opbuffer
  rcall ram_read_bytes

  rcall ram_end

  ; set X to op buffer for execute
  ldi XL, low(op_buffer)
  ldi XH, high(op_buffer)

  ; assume normalcy
  cbr r_flags, (1<<f_jump)|(1<<f_end)

  ; go
  rcall execute_statement

  ; pop line number back for error report
  pop r17
  pop r16

  ; restore linemap pointer
  pop ZH
  pop ZL

  ; error check
  tst r_error
  breq PC+3

  ; error
  set ; include line number
  rjmp handle_error

  ; program over?
  sbrc r_flags, f_end
  ret

  ; if we're doing a jump, then we don't need the normal advance
  sbrc r_flags, f_jump

  ; new target is in r20:r21, reload and go
  rjmp exec_linemap_load

  ; restore linemap page
  mov r21, ZH
  ldi ZH, linemap_buffer_h

  ; see if we've gone off the end of the page
  tst ZL
  brne exec_linemap_next

  ; yep, load the next page
  inc r21
  clr r20
  rjmp exec_linemap_load


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

  ; run line to the end unless told otherwise
  cbr r_flags, 1<<f_abort_line

  ; and jump to the handler
  icall

  ; error check and abort
  tst r_error
  breq PC+2
  ret

  ; break check
  rcall usart_rx_byte_maybe
  brtc PC+5
  cpi r16, 0x1b
  brne PC+3
  ldi r_error, error_break
  ret

  ; did op ask as to abort?
  sbrc r_flags, f_abort_line
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
  rjmp op_for     ; 0x08 FOR var = expression TO expression
  rjmp op_next    ; 0x09 NEXT var
  rjmp op_new     ; 0x0a NEW
  rjmp op_clear   ; 0x0b CLEAR
  rjmp op_list    ; 0x0c LIST
  rjmp op_run     ; 0x0d RUN
  rjmp op_end     ; 0x0e END
  rjmp op_on      ; 0x0f [ON]
  rjmp op_off     ; 0x10 [OFF]
  rjmp op_sleep   ; 0x11 [SLEEP]
  rjmp op_reset   ; 0x12 [RESET]
  rjmp op_cls     ; 0x13 [CLS]

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

  ; assume match will fail; flag to execute that it should abort line
  ; this is ignored if it passes, because we jump straight into
  ; execute_statement, which will run the rest of the line
  sbr r_flags, 1<<f_abort_line

  brtc comp_number

  ; prep for walk over each string
  movw YL, r18
  movw ZL, r16

  ld r4, Y+
  ld r5, Z+
  cp r4, r5
  brne PC+4
  tst r4
  brne PC-5
  rjmp comp_eq
  brlt comp_lt
  rjmp comp_gt

comp_number:
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

  ; setup ref to current linemap page (high byte)
  ldi r21, high(linemap_base)

goto_linemap_load:
  ; load page #r21 of the linemap
  clr r16
  mov r17, r21
  clr r18
  rcall ram_read_start

  clr ZL
  ldi ZH, linemap_buffer_h
  clr r16
  rcall ram_read_bytes
  rcall ram_end

  ; reset to start of buffer
  clr ZL
  ldi ZH, linemap_buffer_h

  ; walk the linemap, looking for our line

goto_linemap_next:
  ; load the line number, just to see if we hit the end of program
  ld r16, Z+
  ld r17, Z+

  ; if we reached the end, then it wasn't found
  tst r16
  brne PC+5
  tst r17
  brne PC+3

  ldi r_error, error_no_such_line
  ret

  ; compare the line number we're looking for with the current one
  cp r4, r16
  cpc r5, r17
  brne goto_linemap_advance

  ; found! move Z back to start of linemap entry
  sbiw ZL, 2

  ; linemap offset in extram page #r21 to r20 for mainloop jump
  mov r20, ZL

  ; abort rest of line and trigger jump
  sbr r_flags, (1<<f_abort_line)|(1<<f_jump)

  ret

goto_linemap_advance:
  ; skip opmem pointer
  adiw ZL, 2

  ; see if we've gone off the end of the page
  tst ZL
  brne goto_linemap_next

  ; yep, load the next page
  inc r21
  rjmp exec_linemap_load


op_input:

  ; get the var name
  ld r16, X+

  ; done all vars?
  tst r16
  brne PC+2
  ret

  push XL
  push XH
  push r16

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
  pop r16
  pop XH
  pop XL
  rjmp op_input

  pop r16
  push r16

  bst r16, 7
  brtc op_input_number

  ; scan and count length
  clr r17
  movw YL, XL
  inc r17
  ld r16, Y+
  tst r16
  brne PC-3

  movw ZL, XL
  pop r16

  rcall set_variable

  pop XH
  pop XL

  rjmp op_input

op_input_number:
  ; parse the value
  rcall parse_number

  pop r16
  pop XH
  pop XL

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

  ; figure out the value
  rcall eval_expression

  ; get name back
  pop r18
  mov r19, r18

  ; consider type. var bit 7 and T must be both set (string) or both clear (number)
  andi r18, 0x80
  bld r18, 6
  tst r18
  breq op_let_number
  cpi r18, 0xc0
  breq op_let_string

  ldi r_error, error_type_mismatch
  ret

op_let_string:

  push XL
  push XH

  ; string pointer
  movw XL, r16

  ; scan and count length
  clr r17
  movw YL, XL
  inc r17
  ld r16, Y+
  tst r16
  brne PC-3

  ; get varname back
  mov r16, r19

  ; start of buffer
  movw ZL, XL

  rcall set_variable

  pop XH
  pop XL

  ret

op_let_number:

  ; push number
  sts input_buffer,   r16
  sts input_buffer+1, r17

  ; ready call to set variable
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  mov r16, r19
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

  ; next linemap position in r20:r21, stack it

  ; stack the next line pointer
  ldi ZH, high(gosub_stack)
  st Z+, r20
  st Z+, r21

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
  ld r21, -Z
  ld r20, -Z

  ; save the new stack pointer back
  mov r_gosub_sp, ZL

  ; abort line and trigger jump
  sbr r_flags, (1<<f_abort_line)|(1<<f_jump)

  ret


op_for:

  sbi PORTB, PB0
  rjmp PC


op_next:

  sbi PORTB, PB0
  rjmp PC


op_new:

  ; reset opmem and linemap
  ldi r16, low(opmem_base)
  ldi r17, high(opmem_base)
  sts r_opmem_top_l, r16
  sts r_opmem_top_h, r17

  ; zero linemap in external ram
  clr r16
  ldi r17, high(linemap_base)
  clr r18
  rcall ram_write_start
  clr r16
  clr r17
  rcall ram_write_pair
  rcall ram_end

  ; clear all flags
  clr r_flags

  ; reset random number generator
  clr r16
  sts rand_h, r16
  inc r16
  sts rand_l, r16

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

  ; setup ref to current linemap page (high byte)
  ldi r21, high(linemap_base)

list_linemap_load:
  ; load page #r21 of the linemap
  clr r16
  mov r17, r21
  clr r18
  rcall ram_read_start

  clr ZL
  ldi ZH, linemap_buffer_h
  clr r16
  rcall ram_read_bytes
  rcall ram_end

  ; reset to start of buffer
  clr ZL
  ldi ZH, linemap_buffer_h

  ; walk the linemap, looking for the right place to put this

list_linemap_next:
  ; load the stored line number
  ld r16, Z+
  ld r17, Z+

  ; if we've found line zero, we're done
  tst r16
  brne PC+4
  tst r17
  brne PC+2

  ret

  ; load opmem pointer
  ld r4, Z+
  ld r5, Z+

  ; save position in linemap
  movw YL, ZL

  ; output line number
  push XL
  push XH
  ldi XL, low(input_buffer)
  ldi XH, high(input_buffer)
  rcall format_number
  pop XH
  pop XL
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  rcall usart_print
  ldi r16, ' '
  rcall usart_tx_byte
  rcall usart_tx_byte

  ; set up a buffer for the op
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)

  ; load opmem into buffer
  movw r16, r4
  clr r18
  rcall ram_read_start

  ; read length
  rcall ram_read_byte
  mov r18, r16

  ; read the op
  rcall ram_read_bytes

  rcall ram_end

  ; dump the op
  ldi ZL, low(input_buffer)
  ldi ZH, high(input_buffer)
  mov r16, r18
  clr r17
  rcall usart_tx_bytes_hex

  ; restore linemap pointer
  movw ZL, YL

  ; see if we've gone off the end of the page
  tst ZL
  brne list_linemap_next

  ; yep, load the next page
  inc r21
  rjmp list_linemap_load


op_run:
  rcall op_clear

  push XL
  push XH
  rcall execute_program
  pop XH
  pop XL

  ; abort line, for whatever its worth
  sbr r_flags, 1<<f_abort_line
  ret


op_end:

  ; abort line
  sbr r_flags, (1<<f_abort_line)|(1<<f_end)
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


op_cls:
  ldi ZL, low(text_clear*2)
  ldi ZH, high(text_clear*2)
  rjmp usart_print_static


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

  ; top of string buffer, for creating strings
  ldi r16, low(string_buffer)
  ldi r17, high(string_buffer)
  movw r8, r16

  ; r21 tracks eval state
  ; bit 1: numeric expression --- set on first operand
  ; bit 2: string expression  -/
  clr r21

eval_next:

  ; get expr micro-op
  ld r16, X+

  ; terminator
  tst r16
  brne eval_op_setup

  ; pop result!
  ld r17, -Y
  ld r16, -Y

  ; indicate result type to caller
  bst r21, 2

  ret

eval_op_setup:

  ; XXX can't happen? found something on the stack we weren't expecting
  cpi r16, expr_op_LAST
  brlo PC+3
  rcall usart_tx_byte_hex
  rjmp blink_forever

  ; setup op table pointer
  ldi ZL, low(eval_op_table)
  ldi ZH, high(eval_op_table)

  ; offset into table
  add ZL, r16
  brcc PC+2
  inc ZH

  icall

  tst r_error
  breq eval_next
  ret

eval_op_table:
  .dw 0
  rjmp eval_op_number   ; pop number, set numeric expression
  rjmp eval_op_string   ; pop string, set string expression
  rjmp eval_op_variable ; expand variable, set matching expression type
  rjmp eval_op_add      ; pop two, add, push
  rjmp eval_op_subtract ; pop two, subtract, push
  rjmp eval_op_multiply ; pop two, multiply, push
  rjmp eval_op_divide   ; pop two, divide, push
  rjmp eval_op_abs      ; pop one, fix, push
  rjmp eval_op_rnd      ; rng, push


eval_op_number:

  ; numeric expression
  sbr r21, 0x2

  ; push number onto stack
  ld r16, X+
  ld r17, X+
  st Y+, r16
  st Y+, r17

  ret


eval_op_string:

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

  ret


eval_op_variable:

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

  ret

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

  bst r21, 1
  brts eval_push_numeric_var

  ; string expression
  sbr r21, 0x4

  ; pull Z back to start of string (#r18 bytes back)
  sub ZL, r18
  brcc PC+2
  dec ZH

  ; push pointer to string in variable
  st Y+, ZL
  st Y+, ZH

  ret

eval_push_numeric_var:

  ; numeric expression
  sbr r21, 0x2

  ; push its value
  ld r16, -Z
  ld r17, -Z
  st Y+, r17
  st Y+, r16

  ret


eval_op_add:

  ; pop B
  ld r19, -Y
  ld r18, -Y

  ; pop A
  ld r17, -Y
  ld r16, -Y

  bst r21, 2
  brts eval_add_string

  ; A + B
  add r16, r18
  adc r17, r19
  brvc PC+3

  ldi r_error, error_overflow
  ret

  ; push result
  st Y+, r16
  st Y+, r17

  ret

eval_add_string:

  ; save expression position
  push XL
  push XH

  ; ready new string pointer
  movw XL, r8

  ; push pointer to start of string we're about to create
  st Y+, XL
  st Y+, XH

  ; pointer to first string
  movw ZL, r16

  ; copy first string
  ld r16, Z+
  tst r16
  breq PC+3
  st X+, r16
  rjmp PC-4

  ; pointer to second string
  movw ZL, r18

  ; copy first string
  ld r16, Z+
  tst r16
  breq PC+3
  st X+, r16
  rjmp PC-4

  ; trailing null
  clr r16
  st X+, r16

  ; save position for next string
  movw r8, XL

  ; restore expression position
  pop XH
  pop XL

  ret


eval_op_subtract:

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

  ret


eval_op_multiply:

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

  ret


eval_op_divide:

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop B
  ld r19, -Y
  ld r18, -Y

  tst r18
  brne PC+5
  tst r19
  brne PC+3

  ldi r_error, error_division_by_zero
  ret

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

  ret


eval_op_abs:

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  ; pop A
  ld r17, -Y
  ld r16, -Y

  ; see if we need to negate?
  tst r17
  brpl PC+5

  ; do so!
  com r17
  neg r16
  ldi r18, 0xff
  sbc r17, r18

  ; push result
  st Y+, r16
  st Y+, r17

  ret


eval_op_rnd:

  bst r21, 1
  brts PC+3
  ldi r_error, error_type_mismatch
  ret

  lds r16, rand_l
  lds r17, rand_h

  ; Xorshift (Marsaglia 2003) variant RNG
  ; adapted from http://www.retroprogramming.com/2017/07/xorshift-pseudorandom-numbers-in-z80.html
  mov r18, r17
  lsr r18
  mov r18, r16
  ror r18
  eor r18, r17
  mov r17, r18
  ror r18
  eor r18, r16
  mov r16, r18
  eor r18, r17
  mov r17, r18

  sts rand_l, r16
  sts rand_h, r17

  st Y+, r16
  st Y+, r17

  ret


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

  ; gonna reuse slot, so back Y up to the start
  adiw YL, 1

  ; by default
  clt

  ; see if size matches; if it does, we can just reuse it
  cp r17, r19
  brne PC+3

  ; reusing slot
  clt
  rjmp store_variable

  ; close this slot

  push YL
  push YH

  ; find start of next slot
  movw XL, YL
  sbiw XL, 2 ; overhead
  sub XL, r19
  brcc PC+2
  dec XH

  ; predec, so advance
  adiw XL, 1
  adiw YL, 1

  ; move X->Y
  ld r18, -X
  st -Y, r18
  dec r19
  brpl PC-3

  pop YH
  pop YL

  rjmp consider_variable

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

  sbi PORTB, PB0

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

  cbi PORTB, PB0

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


; receive a byte from the usart if there's one waiting
; outputs:
;   T: set if something was read, clear otherwise
;   r16: received byte, if there was one
usart_rx_byte_maybe:
  clt
  sbis UCSRA, RXC
  ret
  in r16, UDR
  set
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
;   r16:r17: number of bytes to transmit
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


; begin read from SRAM
; inputs
;   r16:r17:r18: 24-bit address
ram_read_start:
  ldi r19, 0x3 ; READ
  rjmp ram_start

; begin write to SRAM
; inputs
;   r16:r17:r18: 24-bit address
ram_write_start:
  ldi r19, 0x2 ; WRITE

  ; fall through

; start SRAM read/write op
;   r16:r17:r18: 24-bit address
;   r19: command (0x2 read, 0x3 write)
ram_start:

  ; pull /CS low to enable device
  cbi PORTB, PB2

  ; send command
  out SPDR, r19
  sbis SPSR, SPIF
  rjmp PC-1

  ; send address
  out SPDR, r18
  sbis SPSR, SPIF
  rjmp PC-1
  out SPDR, r17
  sbis SPSR, SPIF
  rjmp PC-1
  out SPDR, r16
  sbis SPSR, SPIF
  rjmp PC-1

  ret

ram_end:
  ; drive /CS high to indicate end of operation
  sbi PORTB, PB2
  ret

; pull stuff from SRAM, previously set up with ram_read_start
;   r16: number of bytes to read
;   Z: where to store it
ram_read_bytes:
  out SPDR, r16
  sbis SPSR, SPIF
  rjmp PC-1
  in r17, SPDR
  st Z+, r17
  dec r16
  brne ram_read_bytes
  ret

; read single byte from SRAM, previously set up with ram_read_start
;   r16: byte read
ram_read_byte:
  out SPDR, r16
  sbis SPSR, SPIF
  rjmp PC-1
  in r16, SPDR
  ret

; write stuff to SRAM, previously set up with ram_write_start
;   r16: number of bytes to write
;   Z: pointer to stuff to write
ram_write_bytes:
  ld r17, Z+
  out SPDR, r17
  sbis SPSR, SPIF
  rjmp PC-1
  dec r16
  brne ram_write_bytes
  ret

; write single byte to SRAM, previously set up with ram_write_start
;   r16: byte to write
ram_write_byte:
  out SPDR, r16
  sbis SPSR, SPIF
  rjmp PC-1
  ret

; write two bytse to SRAM, previously set up with ram_write_start
;   r16: first byte to write
;   r17: second byte to write
ram_write_pair:
  out SPDR, r16
  sbis SPSR, SPIF
  rjmp PC-1
  out SPDR, r17
  sbis SPSR, SPIF
  rjmp PC-1
  ret


text_newline:
  .db 0xa, 0xd, 0
text_clear:
  .db 27,"[2J",27,"[H", 0

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
text_error_for_without_to:
  .db "FOR WITHOUT TO", 0
text_error_out_of_memory:
  .db "OUT OF MEMORY", 0
text_error_overflow:
  .db "OVERFLOW", 0
text_error_no_such_line:
  .db "NO SUCH LINE", 0
text_error_type_mismatch:
  .db "TYPE MISMATCH", 0
text_error_division_by_zero:
  .db "DIVISION BY ZERO", 0
text_error_invalid_immediate:
  .db "INVALID STATEMENT IN IMMEDIATE MODE", 0
text_error_invalid_program:
  .db "INVALID STATEMENT IN PROGRAM", 0
text_error_break:
  .db "BREAK", 0
