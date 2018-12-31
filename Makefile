all: basic.app bf.app charset.app gfxdemo.app keyboard.app memdump.app porttest.app typetest.app

clean:
	rm *.app

%.app: %.asm lib/screen.asm lib/printf.asm lib/keyb.asm lib/delay.asm lib/splash.asm lib/strap.asm lib/header.asm
	z80asm -o $@ $<

bf.app: bf.asm bf/bfi.asm lib/screen.asm lib/keyb.asm lib/strap.asm lib/header.asm
	z80asm -o $@ $<
