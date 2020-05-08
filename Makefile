PROGRAM := basic

all: $(PROGRAM).hex

clean:
	-rm -f *.hex *.cof *.obj

install: all
	avrdude -v -c arduino -P /dev/ttyACM0 -p m8 -U flash:w:$(PROGRAM).hex:i

%.hex: %.asm
	gavrasm $<

serial:
	tio -b 38400 /dev/ttyUSB0

.PHONY: serial
