all: basic.app charset.app gfxdemo.app keyboard.app memdump.app porttest.app typetest.app

%.app: %.asm lib/screen.asm lib/printf.asm lib/keyb.asm lib/delay.asm lib/splash.asm lib/strap.asm lib/header.asm
	z80asm -o $@ $<
