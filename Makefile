PROGRAM := basic

all: $(PROGRAM).hex

clean:
	-rm -f *.hex *.cof *.obj

install: all
	avrdude -v -c arduino -P /dev/ttyACM0 -p m88p -U flash:w:$(PROGRAM).hex:i

%.hex: %.asm
	avra $<

serial:
	picocom -b 38400 -s 'sx -vv' -v 'rx -vv -E' /dev/ttyUSB0

fuses:
	avrdude -v -c arduino -P /dev/ttyACM0 -p m88p -U efuse:w:0xf9 -U hfuse:w:0xdf -U lfuse:w:0xff:m

.PHONY: install serial fuses

