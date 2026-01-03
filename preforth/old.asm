BITS 16
org 0x500

; BP = PSP (parameter stack pointer)
; SP = RSP (return stack pointer)


%macro	pspush	1		; src
	sub	bp, 2
	mov	word [bp], %1
%endmacro

%macro	pspop	1		; dst
	mov	word %1, [bp]
	add	bp, 2
%endmacro

%macro	defword	3		; name, len, link
	db	%1
	dw	%3
	db	%2
%endmacro

%define	NUL	0


start:		cld				; clear direction flag
		jmp	abort

		; ( -- )
		defword	'quit', 4, 0
quit:		mov	sp, 0x0000
		jmp	interpret

		; ( -- )
		defword 'abort', 5, quit
abort:		mov	bp, 0xff00
		jmp	quit

		; ( a -- )
		defword 'execute', 7, abort
execute:	pspop	ax
		jmp	ax

		; ( -- )
		defword 'interpret', 9, execute
interpret:	call	word_
		call	dot
		call	space
		call	dot
		call	nl_out
		jmp	interpret
		
in:		dw buffer		; in>
buffer:		times 65 db 0		; in(

		; ( -- sa sl )
word_:		call	in_in
		pspop	ax
		cmp	ax, 0x20
		jbe	word_

		mov	ax, [in]
		dec	ax
		pspush	ax
		xor	cx, cx

	.next:	inc	cx
		call	in_in
		pspop	ax
		cmp	ax, 0x20
		ja	.next

		pspush	cx
		ret


		defword 'in<', 3, NUL
		; ( -- c ) read one char from input buffer, if EOL read new line
in_in:		mov	bx, [in]
		mov	al, [bx]
		xor	ah, ah
		pspush	ax
		test	al, al
		jz	.query
		inc	bx
		mov	[in], bx
		ret

	.query	jmp	query

		defword 'query', 5, NUL
		; ( -- ) fill input buffer
query:		mov	di, buffer

	.next	cmp	di, buffer+64	; buffer full?
		je	.cr		; submit

		call	key
		pspop	ax
		cmp	al, 0x0d
		je	.cr

		cmp	al, 0x08
		je	.bs

		cmp	al, 0x20	; whitespace character?
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

	.cr:	xor	al, al		; append input with null-terminal
		stosb
		mov	bx, in
		mov	word [bx], buffer
		ret

parse:		; ( sa sl -- n? f ) parses string as a number

key:		; ( -- c ) get char c from direct input
		mov	ah, 0x00
		int	0x16
		xor	ah, ah
		pspush	ax
		ret
		

emit:		; ( c -- ) spit char c to output stream
		pspop	ax
		mov	ah, 0x0e
		int	0x10
		ret

		defword "emitln", 6, NUL
		; ( a -- ) 'emit' line at addr a
_emitln:	pspop	si
		mov	cl, 64
	.next:	lodsb
		test	al, al
		jz	.done
		pspush	ax
		call	emit
		test	cl, cl
		jz	.done
		dec	cl
		jmp	.next
	.done:	ret

emitln:		call	in_in	; ( -- c )
		pspop	ax
		test	al, al
		jz	.done
		pspush	ax
		call	emit
		jmp	emitln
	.done:	ret

		defword "nl>", 3, NUL
		; emit newline
nl_out:		pspush	0x0d ; CR
		call	emit
		pspush	0x0a ; LF
		call	emit
		ret

		defword "bs>", 3, NUL
		; ( -- ) delete previous char in TTY mode
bs_out:		pspush	0x08
		call	emit
		pspush	0x20
		call	emit
		pspush	0x08
		call	emit
		ret

space:		pspush 0x20
		call emit
		ret

		defword '.', 1, NUL	; ( n -- )
dot:		pspop ax
	.digit:	xor dx, dx
		mov bx, 10
		div bx		; quotient in ax, remainder in dx
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


nop: jmp nop

; exit
; scnt
; rcnt
; (b)
; (n)
; (br)
; (?br)
; (next)
; c@
; @
; c!
; !
; 1+
; 1-
; +
; -
; <
; <<
; >> ; <<8 ; >>8
; and
; or
; xor
; not
; *
; /mod
; dup
; ?dup
; drop
; swap
; over
; rot
; rot>
; r@
; >r
; r>
; r~
; move
; []=
; find
; [c]?
; a>
; >a
; a>r
; r>a
; a+
; a-
; ac@
; ac!
; ioerr
; 'current
; 'here
; nl
; ln<
; 'emit
; 'key?
; 'curword
; '(wnf)
; 'in(
; 'in>
; inbuf
; current
; here
; emit
; key?
; in(
; in>
; lnsz
; noop
; =
; >
; 0<
; 0>=
; >=
; <=
; 2drop
; 2dup
; nip
; tuck
; =><=
; /
; mod
; <>
; min
; max
; -^
; rshift
; lshift
; l|m
; +!
; ac@+
; ac!+
; leave
; to
; c@+
; c!+
; allot
; fill
; allot0
; immediate
; ,
; c,
; l,
; m,
; move,
; jmpi!
; calli!
; crc16
; stype
; eot
; bs
; lf
; cr
; spc
; spc>
; nl>
; stack?
; litn
; fmtd
; fmtx
; fmtx
; .
; .x
; .x
; parse
; key
; in)
; bs?
; ws?
; lntype
; rdln
; in<?
; in<
; in$
; ,"
; toword
; curword
; word
; word!
; (wnf)
; run1
; nc,
; code
; '?
; '
; forget
; s=
; waitw
; [if]
; [then]
; _bchk
; dump
; psdump
; .s
; (emit)
; (key?)
; bye
; (blk@)
; (blk!)
; blk(
; blk)
; 'blk>
; blk>
; blkdty
; blkin>
; blk$
; blk!
; flush
; blk@
; blk!!
; wipe
; copy
; lnlen
; emitln
; list
; index
; \s
; load
; loadr
; ed
; ve
; me
; archm
; rxtx
; xcomp
; blksz$
; init
; ;code
; create
; doer
; _
; does>
; alias
; value
; values
; consts
; boot
; xtcomp
; :
; if
; then
; else
; (
; \
; s"
; ."
; abort"
; begin
; again
; until
; next
; [
; ]
; compile
; [compile]
; [']
