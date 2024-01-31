%: %.asm
    riscv64-linux-gnu-as $*.asm -o $*.o
    riscv64-linux-gnu-gcc prelude.c $*.o