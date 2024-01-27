wget https://cdimage.ubuntu.com/releases/22.04.3/release/ubuntu-22.04.3-preinstalled-server-riscv64+unmatched.img.xz
xz -dk ubuntu-22.04.3-preinstalled-server-riscv64+unmatched.img.xz
qemu-img resize -f raw ubuntu-22.04.3-preinstalled-server-riscv64+unmatched.img +5G
qemu-system-riscv64 \
-machine virt -nographic -m 2048 -smp 4 \
-bios /usr/lib/riscv64-linux-gnu/opensbi/generic/fw_jump.bin \
-kernel /usr/lib/u-boot/qemu-riscv64_smode/uboot.elf \
-device virtio-net-device,netdev=eth0 -netdev user,id=eth0,hostfwd=tcp::5555-:22 \
-device virtio-rng-pci \
-drive file=ubuntu-22.04.3-preinstalled-server-riscv64+unmatched.img,format=raw,if=virtio
