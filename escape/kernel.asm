BITS 16		; real mode
org 0x500	; offset code

main:
	push ax
	call scan	; print former row
	call pass	; update to latter row
	call copy	; fill former with latter
	call sleep
	pop ax
	dec ax
	cmp ax, 0
	ja main

sleep:
	call tick
	call tick
	call tick
	call tick
	ret

tick:
	mov si, 0x46C	; system timer (~55ms)
	mov bx, [si]
	.loop:
	mov ax, [si]
	cmp ax, bx
	je .loop
	ret

copy:
	mov di, former
	mov si, former + 80
	mov bx, former + 81
	.loop:
	mov al, [bx]
	mov [di], al
	inc bx
	inc di
	cmp di, si
	jb .loop
	ret

pass:
	mov di, former
	mov si, former + 80
	.loop:
	call cell
	inc di
	cmp di, si
	jb .loop
	ret

cell:
	mov al, [di - 1]
	mov bl, [di]
	mov cl, [di + 1]

	shl al, 2
	shl bl, 1
	add al, bl
	add al, cl

	mov ah, 0
	mov bx, rule90
	add bx, ax
	mov al, [bx]

	mov bx, di
	add bx, 81
	mov [bx], al

	ret

scan:
	mov di, former
	mov si, former + 80
	.loop:
	mov al, [di]
	call ascii
	call emit
	inc di
	cmp di, si
	jb .loop
	ret

ascii:
	cmp al, 0
	ja .full
	mov al, ' '
	jmp .done
	.full:
	mov al, '#'
	.done:
	ret

emit:
	mov ah, 0x0e	; write char in teletype mode
	; al = char
	mov bh, 0x00	; page number
	mov bl, 0x0f	; color
	int 0x10
	ret


rule90:
db 0, 1, 0, 1, 1, 0, 1, 0

; video mode: 80x25
db 0
former:
times 40 db 0
db 1
times 40 db 0
latter:
times 80 db 0
