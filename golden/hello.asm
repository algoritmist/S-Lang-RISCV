.section .data
d0: .byte 13, 72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33
.section .text
.global main
main:
addi sp, sp, -8
sd ra, 0(sp)
la t0, d0
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
add a0, t0, zero
call output
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add a0, a0, zero
call halt

halt:
    addi a7, zero, 93
    addi a0, zero, 0
    ecall
    ret

.global input
.global output
.global id
.global outputInt