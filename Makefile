build:
	cargo build --target x86_64-unknown-uefi
	mkdir -p mnt/EFI/BOOT
	cp target/x86_64-unknown-uefi/debug/churos.efi mnt/EFI/BOOT/BOOTX64.EFI

run:
	qemu-system-x86_64 \
		-bios third_party/ovmf/RELEASEX64_OVMF.fd \
		-drive format=raw,file=fat:rw:mnt

.PHONY: build