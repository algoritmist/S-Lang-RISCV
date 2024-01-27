.section .data
.section .text
.global main
max:
bge a0, a1, l0
addi sp, sp, -8
sd ra, 0(sp)
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
add a0, a1, zero
call id
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t0, a0, zero
j l1
l0:
addi sp, sp, -8
sd ra, 0(sp)
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
add a0, t1, zero
call id
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t0, a0, zero
l1:
add a0, t0, zero
ret
main:
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi t0, zero, 5
addi t1, zero, 42
add t2, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
add a0, t0, zero
add a1, t1, zero
call max
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t2, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
add a0, t2, zero
call outputInt
ld t2, 0(sp)
addi sp, sp, 8
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