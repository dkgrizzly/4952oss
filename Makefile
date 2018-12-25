all: attrdemo.app charset.app demotime.app gfxdemo.app keyboard.app porttest.app typetest.app basic.app

%.app: %.asm lib/screen.asm lib/printf.asm lib/keyb.asm lib/delay.asm lib/splash.asm lib/strap.asm lib/header.asm
	z80asm -o $@ $<
