all: tetris.app porttest.app keyboard.app demotime.app basic.app

%.app: %.asm lib/screen.asm lib/printf.asm lib/keyb.asm lib/delay.asm
	z80asm -o $@ $<
