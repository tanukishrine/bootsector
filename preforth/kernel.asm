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

; WORD STRUCTURE

%macro wordinit 2
	db	%1	; name (n bytes)
	dw	0	; link (2 bytes)
	db	%2	; len  (1 byte)
	%push dict
	%$link:
%endmacro		; load (n bytes)

%macro wordlink 2
	db	%1	; name (n bytes)
	dw	%$link	; link (2 bytes)
	db	%2	; len  (1 byte)
	%pop
	%push dict
	%$link:
%endmacro		; load (n bytes)

%macro wordlast 2
	db	%1	; name (n bytes)
	dw	%$link	; link (2 bytes)
	db	%2	; len  (1 byte)
%endmacro		; load (n bytes)

%define	FLAG_IMMEDIATE	0x80	; immediate flag
%define	MASK_LENGTH	0x7f	; namelen mask


start:		jmp	abort


; SYSTEM VARIABLES

state:		db 0
latest:		dw nop
here:		dw end
in:		dw buffer
buffer:		times 65 db 0
curchar:	dw buffer
curlen:		db 0


; DICTIONARY

		; key ( -- c )
		; fetch char c from direct input

		wordinit 'key', 3

key:		mov	ah, 0x00
		int	0x16
		xor	ah, ah
		pspush	ax
		ret


		; emit ( c -- )
		; spit char c to output stream

		wordlink 'emit', 4

emit:		pspop	ax
		mov	ah, 0x0e
		int	0x10
		ret


		; abort ( -- )
		; reset PS and jump to quit

		wordlink 'abort', 5

abort:		mov	bp, 0xff00
		jmp	quit


		; quit ( -- )
		; reset RS, clear the buffer,
		; set state to immediate mode,
		; jump to interpret loop

		wordlink 'quit', 4

quit:		mov	sp, 0x0000
		mov	[in], buffer+64
		mov	[state], 0

;		xor	ax, ax
;		mov	cx, 64
;		mov	di, buffer
;		rep	stosb

		jmp	interpret


		; interpret ( -- )
		; main interpret loop
wordlink 'interpret', 9

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

		wordlink 'stype', 5

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

		wordlink 'word', 4

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

		wordlink 'curword', 7

curword:	mov	ax, [curchar]
		pspush	ax
		mov	al, [curlen]
		xor	ah, ah
		pspush	ax
		ret


		; in< ( -- c )
		; read one char from buffered input
		; if end of input, read new line

		wordlink 'in<', 3

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

		wordlink 'rdln', 4

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

		wordlink 'parse', 5

parse:		pspop	cx
		pspop	si
		xor	ax, ax	; current char
		xor	bx, bx	; result

		lodsb
		dec	cx
		cmp	al, "'"
		je	.char
		cmp	al, '$'
		je	.hex
		cmp	al, '-'
		je	.neg
		inc	cx
		jmp	.skip

		; character literal
	.char:	cmp	cx, 2
		jne	.nan

		lodsb
		mov	bl, al
		lodsb
		cmp	al, "'"
		jne	.nan

		jmp	.num

		; hexadecimal
	.hex:	lodsb
		shl	bx, 4		; multiply by 16

		cmp	al, '0'
		jb	.nan
		cmp	al, '9'
		jbe	.digit

		cmp	al, 'a'
		jb	.nan
		cmp	al, 'f'
		jbe	.alpha

		jmp	.nan

	.digit:	sub	al, '0'
		jmp	.add

	.alpha:	sub	al, 'a'-10

	.add:	add	bx, ax
		loop	.hex

		jmp	.num

		; negative decimal
	.neg:	lodsb
		mul	bx, 10
		sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		sub	bx, ax
		loop	.neg
		jmp	.num

		; unsigned decimal
	.pos:	lodsb
		mul	bx, 10
	.skip:	sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		add	bx, ax
		loop	.pos

	.num:	pspush	bx
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

		wordlink 'litn', 4

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
		wordlink 'find', 4
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

		wordlink 'execute', 7

execute:	pspop	ax
		jmp	ax


		; compile, ( w -- )
		; append a call to wordref w to here

		wordlink 'compile,', 8

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

		wordlink 'header', 6

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

		wordlink 'create', 6

create:		call	toword		; ( -- sa sl )
		call	header		; ( sa sl -- )
		ret


		; [ ( -- ) IMMEDIATE
		; to immediate mode

		wordlink '[', 1 | FLAG_IMMEDIATE

to_immediate:	mov	byte [state], 0
		ret


		; ] ( -- )
		; to compile mode

		wordlink ']', 1

to_compile:	mov	byte [state], -1
		ret


		; exit ( -- )
		; compile exit from a word

		wordlink 'exit', 4

exit:		mov	al, 0xc3 ; ret
		mov	di, [here]
		stosb
		mov	[here], di
		ret

		; : x ( -- )
		; create a new word definition with name x

		wordlink ':', 1
colon:		call	create
		call	to_compile
		ret

		; ; ( -- )
		; end current word definition

		wordlink ';', 1 | FLAG_IMMEDIATE
semicolon:	call	exit
		call	to_immediate
		ret


		; spc> ( -- ) emit space character

		wordlink 'spc>', 4

spc_out:	pspush	0x20 ; SP
		call	emit
		ret


		; nl> ( -- ) emit newline
	
		wordlink 'nl>', 3

nl_out:		pspush	.nl
		pspush	2
		call	stype
		ret

		; CR, LF
	.nl:	db 0x0d, 0x0a


		; bs> ( -- )
		; delete prev char in TTY mode

		wordlink 'bs>', 3

bs_out:		pspush	.bs
		pspush	3
		call	stype
		ret

		; BS, SPC, BS
	.bs:	db 0x08, 0x20, 0x08


; FLOW

		; ( -- )
		; ignore input until ')' is read
		wordlink '(', 1 | FLAG_IMMEDIATE
paren:		call	toword
		pspop	bx
		cmp	bx, 1
		je	.check
		add	bp, 2	; drop
		jmp	paren
	.check:	pspop	bx
		mov	bl, [bx]
		cmp	bl, ')'
		jne	paren
		ret

		; ( -- )
		; ignore input until rdln is called
		wordlink '\', 1 | FLAG_IMMEDIATE
backslash:	mov	[in], buffer+64
		ret


; SYSTEM VARIABLES

		; ( -- a )
		wordlink 'here', 4
here_addr:	pspush	here
		ret

		; ( -- a )
		wordlink 'a', 1
a_addr:		pspush	a
		ret

		; ( -- a )
		wordlink 'b', 1
b_addr:		pspush	b
		ret

a: db 'world'
b: db 'abcdefghijklmnopqrstuvwxyz'


; PARAMETER STACK

		; ( a -- )
		wordlink 'drop', 4
drop:		add	bp, 2
		ret

		; ( a -- a a )
		wordlink 'dup', 3
dup:		mov	ax, [bp]
		pspush	ax
		ret

		; ( a -- a a? )
		; dup if a is nonzero
		wordlink '?dup', 4
?dup:		mov	ax, [bp]
		test	ax, ax
		jz	.zero
		pspush	ax
	.zero:	ret

		; ( a b -- b )
		wordlink 'nip', 3
nip:		pspop	ax
		mov	[bp], ax
		ret

		; ( a b -- a b a )
		wordlink 'over', 4
over:		mov	bx, bp
		add	bx, 2
		mov	bx, [bx]
		pspush	bx
		ret

		; ( a b c -- b c a )
		wordlink 'rot', 3
rot:		pspop	ax
		pspop	bx
		mov	cx, [bp]
		mov	[bp], bx
		pspush	ax
		pspush	cx
		ret

		; ( a b c -- c a b )
		wordlink 'nrot', 4
nrot:		pspop	ax
		pspop	bx
		mov	cx, [bp]
		mov	[bp], ax
		pspush	cx
		pspush	bx
		ret

		; ( a b -- b a )
		wordlink 'swap', 4
swap:		pspop	ax
		mov	bx, [bp]
		mov	[bp], ax
		pspush	bx
		ret

		; ( a b -- b a b )
		wordlink 'tuck', 4
tuck:		pspop	ax
		mov	bx, [bp]
		mov	[bp], ax
		pspush	bx
		pspush	ax
		ret

		; ( a a -- )
		wordlink '2drop', 5
twodrop:	add	bp, 4
		ret

		; ( a b -- a b a b )
		wordlink '2dup', 4
twodup:		pspop	ax
		mov	bx, [bp]
		sub	bp, 4
		mov	[bp], bx
		pspush	ax
		ret


; RETURN STACK

		; ( n -- R:n )
		wordlink '>r', 2
to_r:		pspop	ax
		pop	bx
		push	ax
		push	bx
		ret

		; ( R:n -- n )
		wordlink 'r>', 2
r_from:		pop	ax
		pop	bx
		push	ax
		pspush	bx
		ret

		; ( R:n -- n R:n )
		wordlink 'r@', 2
r_fetch:	pop	ax
		pop	bx
		pspush	bx
		push	bx
		push	ax
		ret

		; ( R:n -- )
		wordlink 'rdrop', 5
rdrop:		pop	ax
		pop	bx
		push	ax
		ret


; STACK META

		; .s ( -- )
		; prints contents of the stack

		wordlink '.s', 2

dot_s:		call	scnt
		pspop	cx
		shr	cx, 1
		mov	si, 0xfefe
		std

	.next:	test	cx, cx
		jz	.done
		dec	cx
		lodsw
		pspush	ax
		call	spc_out
		call	dot
		jmp	.next

	.done:	cld
		ret


		; scnt ( -- n )
		; size of PS in bytes

		wordlink 'scnt', 4

scnt:		mov	ax, 0xff00
		sub	ax, bp
		pspush	ax
		ret


		; rcnt ( -- n )
		; size of RS in bytes

		wordlink 'rcnt', 4

rcnt:		mov	ax, 0x0000
		sub	ax, sp
		pspush	ax
		ret


; MEMORY

		; ( a -- n )
		; fetch value stored at addr a
		wordlink '@', 1
fetch:		mov	bx, [bp]
		mov	bx, [bx]
		mov	[bp], bx
		ret

		; ( n a -- )
		; store n at addr a
		wordlink '!', 1
store:		pspop	bx
		pspop	ax
		mov	[bx], ax
		ret

		; ( n -- )
		; write n in here and advance it
		wordlink ',', 1
comma:		pspop	ax
		mov	di, [here]
		stosw
		mov	[here], di
		ret

		; ( n a -- )
		; increase value at addr a by n
		wordlink '+!', 2
plus_store:	pspop	bx
		pspop	ax
		add	[bx], ax
		ret

		; ( a1 a2 u -- f )
		; compare u bytes between a1 and a2
		wordlink '[]=', 3
scmp:		pspop	cx
		pspop	si
		pspop	di
		repe	cmpsb
		jz	.true
		pspush	0
		ret
	.true:	pspush	-1
		ret

		; ( c a u -- i )
		; look for c within u bytes at addr a
		wordlink '[c]?', 4
c_find:		pspop	cx
		pspop	si
		pspop	ax
		xor	bx, bx
	.next	cmp	[si], al
		je	.true
		inc	bx
		inc	si
		loop	.next
		pspush	-1
		ret
	.true	pspush	bx
		ret

		; ( a -- c )
		; fetch byte c stored at addr a
		wordlink 'c@', 2
c_fetch:	mov	bx, [bp]
		mov	bl, [bx]
		xor	bh, bh
		mov	[bp], bx
		ret

		; ( a -- a+1 c )
		; fetch byte c stored at addr a and inc a
		wordlink 'c@+', 3
c_fetch_plus:	mov	bx, [bp]
		mov	al, [bx]
		xor	ah, ah
		inc	bx
		mov	[bp], bx
		pspush	ax
		ret

		; ( c a -- )
		; store byte c in addr a
		wordlink 'c!', 2
c_store:	pspop	bx
		pspop	ax
		mov	[bx], al
		ret

		; ( c a -- a+1 )
		; store byte c in addr a and inc a
		wordlink 'c!+', 3
c_store_plus:	pspop	bx
		mov	ax, [bp]
		mov	[bx], al
		inc	bx
		mov	[bp], bx
		ret

		; ( c -- )
		; store byte c in here and advance it
		wordlink 'c,', 2
c_comma:	pspop	ax
		mov	bx, [here]
		mov	[bx], al
		inc	bx
		mov	[here], bx
		ret

		; ( n -- )
		; move here by n bytes
		wordlink 'allot', 5
allot:		pspop	ax
		mov	bx, [here]
		add	bx, ax
		mov	[here], bx
		ret

		; ( n -- )
		; allot n bytes and fill with zero
		wordlink 'allot0', 6
allot0:		pspop	cx
		mov	di, [here]
		xor	ax, ax
		rep	stosb
		mov	[here], di
		ret

		; ( a n c -- )
		; fill n bytes at addr a with char c
		wordlink 'fill', 4
fill:		pspop	ax
		pspop	cx
		pspop	di
		rep	stosb
		ret

		; ( a1 a2 u -- )
		; copy u bytes from a1 to a2
		wordlink 'move', 4
move:		pspop	cx
		pspop	di
		pspop	si
		rep	movsb
		ret

		; ( a u -- )
		; copy u bytes from a to here
		wordlink 'move,', 5
move_comma:	pspop	cx
		pspop	si
		mov	di, [here]
		rep	movsb
		ret


; ARITHMETIC / BITS

		; ( a b -- a+b )
		wordlink '+', 1
plus:		pspop	bx
		mov	ax, [bp]
		add	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a-b )
		wordlink '-', 1
minus:		pspop	bx
		mov	ax, [bp]
		sub	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- b-a )
		wordlink '-^', 2
minus_opp:	pspop	bx
		mov	ax, [bp]
		sub	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a*b )
		wordlink '*', 1
mul:		pspop	bx
		mov	ax, [bp]
		mul	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a/b )
		wordlink '/', 1
div:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], ax
		ret

		; ( n1 n2 -- lo hi )
		wordlink '<>', 2
sort:		pspop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.skip
		xchg	ax, bx
		mov	[bp], ax
	.skip	pspush	bx
		ret

		; ( n -- n*2 )
		wordlink '2*', 2
shiftl:		mov	ax, [bp]
		shl	ax, 1
		mov	[bp], ax
		ret

		; ( n -- n/2 )
		wordlink '2/', 2
shiftr:		mov	ax, [bp]
		shr	ax, 1
		mov	[bp], ax
		ret

		; ( n -- n+1 )
		wordlink '1+', 2
oneplus:	mov	ax, [bp]
		inc	ax
		mov	[bp], ax
		ret

		; ( n -- n-1 )
		wordlink '1-', 2
oneminus:	mov	ax, [bp]
		dec	ax
		mov	[bp], ax
		ret

		; ( n -- n+2 )
		wordlink '2+', 2
twoplus:	mov	ax, [bp]
		add	ax, 2
		mov	[bp], ax
		ret

		; ( n -- n-2 )
		wordlink '2-', 2
twominus:	mov	ax, [bp]
		sub	ax, 2
		mov	[bp], ax
		ret

		; ( n1 n2 -- hi )
		wordlink 'max', 3
max:		pspop	bx
		mov	ax, [bp]
		cmp	bx, ax
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( n1 n2 -- lo )
		wordlink 'min', 3
min:		pspop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( a b -- a%b )
		wordlink 'mod', 3
mod:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		ret

		; ( a b -- r q )
		wordlink '/mod', 4
divmod:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		pspush	ax
		ret

		; ( a b -- a&b )
		wordlink 'and', 3
and:		pspop	bx
		mov	ax, [bp]
		and	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a|b )
		wordlink 'or', 2
or:		pspop	bx
		mov	ax, [bp]
		or	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a^b )
		wordlink 'xor', 3
xor:		pspop	bx
		mov	ax, [bp]
		xor	ax, bx
		mov	[bp], ax
		ret

		; ( n u -- n<<u )
		wordlink 'lshift', 6
lshift:		pspop	cx
		mov	ax, [bp]
		shl	ax, cx
		mov	[bp], ax
		ret

		; ( n u -- n>>u )
		wordlink 'rshift', 6
rshift:		pspop	cx
		mov	ax, [bp]
		shr	ax, cx
		mov	[bp], ax
		ret


; OTHER...

		; . ( n -- )
		; print n in its unsigned decimal form

		wordlink 'u.', 2

udot:		pspop	ax

	digit:	xor	dx, dx
		mov	bx, 10
		div	bx
		test	ax, ax
		jz	.zero
		push	dx
		call	digit
		pop	dx
	.zero:	push	ax
		mov	al, dl
		add	al, '0'
		pspush	ax
		call	emit
		pop	ax
		ret


		; ( n -- )
		; print n in its signed decimal form

		wordlink '.', 1

dot:		pspop	ax
		test	ax, ax	; sign?
		jns	digit

		not	ax
		inc	ax
		push	ax

		pspush	'-'
		call	emit

		pop	ax
		jmp	digit


		wordlast 'nop', 3
nop:		ret

end:		db 'hello world'
