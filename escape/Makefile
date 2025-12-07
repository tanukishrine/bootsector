.PHONY: run clean

build: disk.img

run: disk.img
	qemu-system-i386 -hda disk.img

DO NOT RUN ON ANY OTHER MACHINES THAN MINE
load: build
	truncate -s 2048 disk.img
	diskutil unmountDisk /dev/disk4
	sudo dd if=disk.img of=/dev/rdisk4 bs=512 status=progress
	sync
	diskutil eject /dev/disk4

disk.img: bootloader.img kernel.img
	cat bootloader.img kernel.img > $@

bootloader.img: bootloader.asm
	nasm -o $@ bootloader.asm

kernel.img: kernel.asm
	nasm -o $@ kernel.asm

clean:
	rm -f bootloader.img kernel.img disk.img
