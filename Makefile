PROGRAM := basic

all: $(PROGRAM).hex

clean:
	-rm -f *.hex *.cof *.obj

install: all
	avrdude -v -c arduino -P /dev/ttyACM0 -p m8 -U flash:w:$(PROGRAM).hex:i

%.hex: %.asm
	avra $<

serial:
	picocom -b 38400 -s 'sx -vv' -v 'rx -vv -E' /dev/ttyUSB0

.PHONY: install serial
