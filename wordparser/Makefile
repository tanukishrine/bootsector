.PHONY: run clean

default: clean run

run: disk.img
	qemu-system-i386 -hda disk.img

clean:
	rm *.img

bootloader.img:
	nasm bootloader.asm -o $@

kernel.img:
	nasm kernel.asm -o $@

disk.img: bootloader.img kernel.img
	cat bootloader.img kernel.img > disk.img
