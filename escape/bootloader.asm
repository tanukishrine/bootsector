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

jmp 0:0x500	; jump to payload, zero code segment

; pad with zeros until magic number
times 510 - ($ - $$) db 0
db 0x55, 0xaa
