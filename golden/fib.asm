.section .data
.section .text
.global main
fact:
addi t0, zero, 2
blt a0, t0, l0
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi t2, zero, 1
sub t3, a0, t2
add t4, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd t4, 0(sp)
add a0, t3, zero
call fact
ld t4, 0(sp)
addi sp, sp, 8
ld t3, 0(sp)
addi sp, sp, 8
ld t2, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
mul t5, t4, a0
add t6, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd t6, 0(sp)
addi sp, sp, -8
sd t5, 0(sp)
addi sp, sp, -8
sd t4, 0(sp)
add a0, t5, zero
call id
ld t4, 0(sp)
addi sp, sp, 8
ld t5, 0(sp)
addi sp, sp, 8
ld t6, 0(sp)
addi sp, sp, 8
ld t3, 0(sp)
addi sp, sp, 8
ld t2, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t1, a0, zero
j l1
l0:
addi sp, sp, -8
sd ra, 0(sp)
addi t2, zero, 1
add t3, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
add a0, t2, zero
call id
ld t3, 0(sp)
addi sp, sp, 8
ld t2, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t1, a0, zero
l1:
add a0, t1, zero
ret
main:
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi t0, zero, 10
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
add a0, t0, zero
call fact
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t1, a0, zero
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t0, 0(sp)
add a0, t1, zero
call outputInt
ld t0, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
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