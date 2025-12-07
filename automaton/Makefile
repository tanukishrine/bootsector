.PHONY: run clean

build: bootsector.img

run: build
	qemu-system-i386 -hda bootsector.img

clean:
	rm -f bootsector.img

bootsector.img: bootsector.asm
	nasm -o $@ bootsector.asm

# DO NOT RUN ON ANY OTHER MACHINES THAN MINE
load: build
	diskutil unmountDisk /dev/disk4
	sudo dd if=bootsector.img of=/dev/rdisk4 bs=512
	sync
	diskutil eject /dev/disk4
