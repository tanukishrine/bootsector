BITS 16
org 0x500


; BX = W (working register)
; BP = PSP (parameter stack pointer)
; SP = RSP (return stack pointer)


%define	INT_VIDEO	0x10	; video interrupt
%define	INT_KEYBOARD	0x16	; keyboard interrupt

%define	KEY_WAIT_READ	0x00	; function: wait for keypress and read character
%define	TTY_OUTPUT	0x0e	; function: write text in teletype mode

%define	NUL		0x00	; null
%define BS		0x08	; backspace
%define LF		0x0a	; line feed
%define	CR		0x0d	; carriage return


%macro	pspush	1		; src
	sub	bp, 2
	mov	[bp], %1
%endmacro

%macro	pspop	1		; dst
	mov	%1, [bp]
	add	bp, 2
%endmacro


main:		cld			; clear direction flag
		mov	sp, 0x0000	; set return stack pointer
		mov	bp, 0xf800	; set parameter stack pointer

.loop:		call	readline
		call	newline
		call	printwords
		call	newline
		jmp	.loop


buffer:		times 65 db NUL


readline:	mov	di, buffer

.next:		cmp	di, buffer+64	; buffer full?
		je	.submit		; submit

		mov	ah, KEY_WAIT_READ
		int	INT_KEYBOARD

		cmp	al, BS
		je	.backspace

		cmp	al, CR		; enter?
		je	.submit

		cmp	al, ' '		; non-whitespace character?
		jb	.next		; do nothing

		stosb			; store string byte

		mov	ah, TTY_OUTPUT
		int	INT_VIDEO

		jmp	.next

.backspace:	cmp	di, buffer	; empty buffer?
		je	.next		; do nothing

		dec	di		; clear previous character
		mov	byte [di], NUL

		mov	ah, TTY_OUTPUT	; clear printed character
		int	INT_VIDEO
		mov	al, ' '
		int	INT_VIDEO
		mov	al, BS
		int	INT_VIDEO

		jmp	.next

.submit:	xor	al, al		; append input with null-terminal
		stosb

		ret


printline:	mov	si, buffer

.next:		lodsb			; load string byte

		test	al, al		; null?
		jz	.done		; exit

		mov	ah, TTY_OUTPUT
		int	INT_VIDEO

		jmp	.next

.done:		ret

in:		dw	buffer		; pointer to current word


printwords:	mov	si, buffer

.next:		call	readword
		test	al, al
		jz	.done

		call	newline
		jmp	.next

.done:		ret


readword:				; si = pointer to inside buffer
.skip:		lodsb

		test	al, al		; null?
		jz	.done		; exit

		cmp	al, ' '		; whitespace?
		jbe	.skip

		mov	[in], si	; update IN to curword
		xor	cl, cl		; set counter for curword length

		mov	ah, TTY_OUTPUT
		int	INT_VIDEO

.next:		lodsb

		cmp	al, ' '		; whitespace or null?
		jbe	.done

		mov	ah, TTY_OUTPUT
		int	INT_VIDEO

		inc	cl
		jmp	.next

.done:		ret


newline:	mov	ah, TTY_OUTPUT
		mov	al, CR
		int	INT_VIDEO
		mov	al, LF
		int	INT_VIDEO
		ret
