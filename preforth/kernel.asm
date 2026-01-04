BITS 16
org 0x500

; bp = PSP (parameter stack pointer)
; sp = RSP (return stack pointer)

; FORTH MACROS
%macro	pspush	1	; src
	sub	bp, 2
	mov	word [bp], %1
%endmacro

%macro	pspop	1	; dst
	mov	word %1, [bp]
	add	bp, 2
%endmacro

%macro	pspeek	1	; dst
	mov	word %1, [bp]
%endmacro

; WORD STRUCTURE
%macro	defword	3
	db	%1	; name (n bytes)
	dw	%3	; link (2 bytes)
	db	%2	; len  (1 byte)
%endmacro		; load (n bytes)

%define	FLAG_IMMEDIATE	0x80	; immediate flag
%define	MASK_LENGTH	0x7f	; namelen mask

start:		jmp	abort


; SYSTEM VARIABLES
state:		db 0
latest:		dw scmp
here:		dw end
in:		dw buffer
buffer:		times 65 db 0
curchar:	dw buffer
curlen:		db 0


; DICTIONARY

		; key ( -- c )
		; fetch char c from direct input

		defword 'key', 3, 0

key:		mov	ah, 0x00
		int	0x16
		xor	ah, ah
		pspush	ax
		ret


		; emit ( c -- )
		; spit char c to output stream

		defword 'emit', 4, key

emit:		pspop	ax
		mov	ah, 0x0e
		int	0x10
		ret


		; abort ( -- )
		; reset PS and jump to quit

		defword 'abort', 5, emit

abort:		mov	bp, 0xff00
		jmp	quit


		; quit ( -- )
		; reset RS and clear the buffer
		; and jump to interpret loop

		defword 'quit', 4, abort

quit:		mov	sp, 0x0000

		xor	ax, ax
		mov	cx, 64
		mov	di, buffer
		rep	stosb

		jmp	interpret


		; interpret ( -- )
		; main interpret loop

		defword 'interpret', 9, quit

interpret:	; read next word
		call	toword		; ( -- sa sl )

		; is a number?
		call	parse		; ( sa sl -- n? f )
		pspop	ax		; ( n? f -- n? )
		test	ax, ax
		jnz	.num

		; is a word?
		call	curword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		pspop	ax		; ( w? f -- w? )
		test	ax, ax
		jnz	.word

		; not a word
		call	nl_out		; ( -- )
		call	curword		; ( -- sa sl )
		call	stype		; ( sa sl -- )
		pspush	.err		; ( -- sa )
		pspush	15		; ( -- sa sl )
		call	stype		; ( sa sl -- )
		jmp	abort

	.err:	db ' word not found'

	.num:	mov	al, [state]
		test	al, al
		jz	interpret

		call	litn
		jmp	interpret

	.word:	mov	al, [state]
		test	al, al
		jz	.run

		; immediate word?
		mov	bx, [bp]
		mov	al, [bx-1]
		and	al, FLAG_IMMEDIATE
		test	al, al
		jnz	.run

		call	compile_comma
		jmp	interpret

	.run:	call	execute
		jmp	interpret


		; stype ( sa sl -- )
		; emit all chars of string

		defword 'stype', 5, interpret

stype:		pspop	cx
		pspop	si
		mov	ah, 0x0e
	.next:	lodsb
		int	0x10
		loop	.next
		ret


		; word ( -- sa sl )
		; read one word from buffered input
		; update curchar and curlen

		defword 'word', 4, stype

toword:		call	tochar
		pspop	ax
		cmp	ax, 0x20
		jbe	toword
		mov	ax, [in]
		dec	ax
		pspush	ax
		mov	[curchar], ax
		xor	cx, cx

	.next:	inc	cx
		call	tochar
		pspop	ax
		cmp	ax, 0x20
		ja	.next

		pspush	cx
		mov	[curlen], cl
		ret


		; curword ( -- sa sl )
		; yield the last read word

		defword 'curword', 7, toword

curword:	mov	ax, [curchar]
		pspush	ax
		mov	al, [curlen]
		xor	ah, ah
		pspush	ax
		ret


		; in< ( -- c )
		; read one char from buffered input
		; if end of input, read new line

		defword 'in<', 3, curword

tochar:		mov	bx, [in]
		cmp	bx, buffer+64
		jbe	.skip
		call	rdln
		mov	bx, buffer
	.skip:	mov	al, [bx]
		xor	ah, ah
		pspush	ax
		inc	bx
		mov	[in], bx
		ret


		; rdln ( -- )
		; feed a line to the buffered input

		defword 'rdln', 4, tochar

rdln:		pspush	.ok
		pspush	5
		call	stype

		mov	di, buffer

	.next	cmp	di, buffer+64	; buffer full?
		je	.cr		; submit

		call	key
		pspop	ax

		cmp	al, 0x0d
		je	.cr

		cmp	al, 0x08
		je	.bs

		cmp	al, 0x20	; control character?
		jb	.next		; do nothing

		stosb			; store string byte
		pspush	ax
		call	emit
		jmp	.next

	.bs:	cmp	di, buffer	; empty buffer?
		je	.next		; do nothing

		dec	di		; clear previous character
		mov	byte [di], 0
		call	bs_out
		jmp	.next

	.cr:	xor	al, al		; flush rest of line with null
	.null:	stosb			
		cmp	di, buffer+64
		jbe	.null
		mov	bx, in		; reset input ptr
		mov	[bx], buffer
		ret

	.ok	db ' ok', 13, 10


		; parse ( sa sl -- n? f )
		; convert string as a number

		defword 'parse', 5, rdln

parse:		pspop	cx
		pspop	si
		xor	ah, ah	; current char
		xor	bx, bx	; result

	.next:	lodsb
		mul	bx, 10
		sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		add	bx, ax
		loop	.next
		pspush	bx
		pspush	-1
		ret

	.nan:	pspush	0
		ret


		; lit ( -- n ) runtime

lit:		pop	si
		lodsw
		pspush	ax
		push	si
		ret


		; litn ( n -- )
		; write n as a literal

		defword 'litn', 4, parse

litn:		mov	al, 0xe8	; call opcode
		mov	di, [here]
		stosb

		mov	ax, lit		; relative addressing
		sub	ax, di
		sub	ax, 2
		stosw

		pspop	ax		; store n
		stosw

		mov	[here], di
		ret


		; find ( sa sl -- w? f )
		defword 'find', 4, litn
find:		pspop	ax		; len
		pspop	dx		; addr
		mov	bx, [latest]	; curr

	.loop:	mov	cl, [bx-1]	; same length?
		and	cl, 0x7f	; ignore immediate flag
		cmp	al, cl
		jne	.skip

		mov	si, dx
		mov	di, bx
		sub	di, 3
		sub	di, ax		; beginning of name
		mov	cx, ax
		repz	cmpsb
		jz	.found	

	.skip:	mov	bx, [bx-3]	; next entry
		cmp	bx, 0
		jnz	.loop

		pspush	0
		ret

	.found:	pspush	bx
		pspush	-1
		ret


		; execute ( w -- )
		; jump IP to addr w
		defword 'execute', 7, find
execute:	pspop	ax
		jmp	ax


		; compile, ( w -- )
		; append a call to wordref w to here

		defword 'compile,', 8, execute

compile_comma:	mov	al, 0xe8 ; call opcode
		mov	di, [here]
		stosb

		pspop	ax ; wordref
		sub	ax, di
		sub	ax, 2
		stosw

		mov	[here], di
		ret


		; header ( sa sl -- )
		; append a new header with name s

		defword 'header', 6, compile_comma

header:		pspop	bx
		mov	cx, bx
		pspop	si
		mov	di, [here]
		rep	movsb
		mov	ax, [latest]
		stosw
		mov	al, bl
		stosb
		mov	[latest], di
		mov	[here], di
		ret


		; create x ( -- )
		; append a new header with name x

		defword 'create', 6, header

create:		call	toword		; ( -- sa sl )
		call	header		; ( sa sl -- )
		ret


		; [ ( -- ) IMMEDIATE
		; to immediate mode

		defword '[', 1 | FLAG_IMMEDIATE, create

to_immediate:	mov	byte [state], 0
		ret


		; ] ( -- )
		; to compile mode

		defword ']', 1, to_immediate

to_compile:	mov	byte [state], -1
		ret


		; exit ( -- )
		; compile exit from a word

		defword 'exit', 4, to_compile

exit:		mov	al, 0xc3 ; ret
		mov	di, [here]
		stosb
		mov	[here], di
		ret

		; : x ( -- )
		; create a new word definition with name x

		defword ':', 1, exit
colon:		call	create
		call	to_compile
		ret

		; ; ( -- )
		; end current word definition

		defword ';', 1 | FLAG_IMMEDIATE, colon
semicolon:	call	exit
		call	to_immediate
		ret

		; , ( n -- )
		; write word n to here

		defword 'c,', 2, semicolon

byte_comma:	pspop	ax
		mov	di, [here]
		stosb
		mov	[here], di
		ret


		; , ( c -- )
		; write byte c to here

		defword ',', 1, byte_comma

comma:		pspop	ax
		mov	di, [here]
		stosw
		mov	[here], di
		ret


		; scnt ( -- n ) size of PS in bytes

		defword 'scnt', 4, comma

scnt:		mov	ax, 0xff00
		sub	ax, bp
		pspush	ax
		ret


		; rcnt ( -- n ) size of RS in bytes

		defword 'rcnt', 4, scnt

rcnt:		mov	ax, 0x0000
		sub	ax, sp
		pspush	ax
		ret


		; spc> ( -- ) emit space character

		defword 'spc>', 4, rcnt

spc_out:	pspush	0x20 ; SP
		call	emit
		ret


		; nl> ( -- ) emit newline
	
		defword 'nl>', 3, spc_out

nl_out:		pspush	.nl
		pspush	2
		call	stype
		ret

		; CR, LF
	.nl:	db 0x0d, 0x0a


		; bs> ( -- )
		; delete prev char in TTY mode

		defword	'bs>', 3, nl_out

bs_out:		pspush	.bs
		pspush	3
		call	stype
		ret

		; BS, SPC, BS
	.bs:	db 0x08, 0x20, 0x08


		; . ( n -- )
		; print n in its decimal form

		defword '.', 1, bs_out

dot:		pspop ax
	.digit:	xor dx, dx
		mov bx, 10
		div bx
		cmp ax, 0
		jz .zero
		push dx
		call .digit
		pop dx
	.zero:	push ax
		mov al, dl
		add al, '0'
		pspush ax
		call emit
		pop ax
		ret


		; []= ( a1 a2 u -- f )
		; compare u bytes between a1 and a2

		defword '[]=', 3, dot

scmp:		pspop	cx
		pspop	si
		pspop	di
		repe	cmpsb
		jz	.true
		pspush	0
		ret
	.true:	pspush	-1
		ret


end:		db 237
