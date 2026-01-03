BITS 16
org 0x7c00

	jmp	start

	; FAT headers stops at 3e
	times 0x3e - ( $ - $$ ) db 0

start:
	; clear direction flag
	cld

	; set up hardware stack
	xor	ax, ax
	mov	es, ax
	mov	cs, ax
	mov	ss, ax
	mov	ds, ax
	mov	sp, ax

	; load kernel
	mov	ah, 0x02	; function: read sectors from drive
	mov	al, 59		; sector count (maximum safe limit)
	mov	ch, 0		; cylinder
	mov	cl, 2		; sector
	mov	dh, 0		; head
	mov	bx, 0x500	; destination address
	int	0x13

	; clear screen
	mov	ax, 0x0007	; video mode: 80x25 monochrome text
	int	0x10

	jmp	0x500		; goto: kernel

	; magic numbers
	times 510 - ( $ - $$ ) db 0
	db	0x55, 0xaa
