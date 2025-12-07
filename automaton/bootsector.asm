BITS 16		; real mode
org 0x7c00	; offset code

jmp start	; goto bootloader

; FAT headers (BIOS parameter block)
times 0x3e - ($ - $$) db 0

start:

mov ax, 0
mov ss, ax	; zero stack segment 
mov ds, ax	; zero data segment
mov es, ax	; zero extra segment
mov sp, 0	; setup hardware stack

; LOAD PAYLOAD
mov ah, 0x02	; read disk sectors
mov al, 2	; number of sectors to read
mov ch, 0	; cylinder number
mov cl, 2	; sector number
mov dh, 0	; head number
mov bx, 0x500	; pointer to buffer
int 0x13	; interrupt call

; CLEAR THE SCREEN
mov ah, 0x00	; Set video mode
mov al, 0x03	; 80x25 text mode, color
int 0x10

main:
call scan	; print former row
call pass	; update to latter row
call copy	; fill former with latter
call sleep
jmp main

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
	mov bx, rule
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
	mov al, 32
	jmp .done
	.full:
	mov al, 219
	.done:
	ret

emit:
	mov ah, 0x0e	; write char in teletype mode
	; al = char
	mov bh, 0x00	; page number
	mov bl, 0x0f	; color
	int 0x10
	ret

rule:
db 0, 0, 1, 1, 1, 1, 0, 0

; video mode: 80x25
db 0
former:
db 1
times 39 db 0
db 1, 1, 1, 0, 0, 0, 1, 1, 0, 1
db 0, 0, 1, 0, 0, 0, 0, 0, 1, 0
db 1, 1, 0, 0, 1, 1, 1, 0, 0, 0
db 0, 1, 0, 0, 0, 0, 1, 0, 0, 0

; db 1, 1, 0, 1, 1, 1, 1, 1, 0, 1
; db 1, 0, 0, 1, 0, 1, 1, 0, 1, 1
; db 0, 1, 1, 0, 1, 0, 0, 1, 0, 0

db 0
latter:
times 80 db 0

; pad with zeros until magic number
times 510 - ($ - $$) db 0
db 0x55, 0xaa
