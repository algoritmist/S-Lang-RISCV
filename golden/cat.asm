.section .data
.section .text
.global main
main:
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi t0, zero, 5
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
add a0, t0, zero
call input
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
add a0, t1, zero
call output
ld t1, 0(sp)
addi sp, sp, 8
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