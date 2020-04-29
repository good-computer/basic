.device ATmega8

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
  cli

  ; setup stack pointer
  ldi r16, low(RAMEND)
  ldi r17, high(RAMEND)
  out SPL, r16
  out SPH, r17

  sei

  ; PB1/OC1A output
  ldi r16, (1<<PB1)
  out DDRB, r16

  ; COM1A1          non-inverting mode
  ; WGM11 | WGM10   10bit phase-corrected PWM mode
  ldi r16, (1<<COM1A1) | (1<<WGM11) | (1<<WGM10)
  out TCCR1A, r16

  ; CS11            prescaler to 8
  ldi r16, 1<<CS11
  out TCCR1B, r16

  ; only want 8-bit timer
  ldi r16, 0
  out OCR1AH, r16


main:
  
  ldi r16, 0

up:
  out OCR1AL, r16

  ; ~10ms
  ldi  r18, 208
  ldi  r19, 202
L1: dec  r19
  brne L1
  dec  r18
  brne L1

  inc r16
  brne up

  ldi r16, 255

down:
  out OCR1AL, r16

  ; ~10ms
  ldi  r18, 208
  ldi  r19, 202
L2: dec  r19
  brne L2
  dec  r18
  brne L2

  dec r16
  brne down

  rjmp main
