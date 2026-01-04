.PHONY:

default: clean build run

clean:
	rm *.img

build: disk.img

run: 
	qemu-system-i386 -hda disk.img

disk.img: bootloader.img kernel.img
	cat bootloader.img kernel.img > disk.img

bootloader.img:
	nasm bootloader.asm -o $@

kernel.img:
	nasm kernel.asm -o $@

load: build
	truncate -s 4096 disk.img
	diskutil unmountDisk /dev/disk4
	sudo dd if=disk.img of=/dev/rdisk4 bs=512 status=progress
	sync
	diskutil eject /dev/disk4
