.section .data
.section .text
.global main
gcd:
addi t0, zero, 0
beq a0, t0, l0
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
rem t2, a1, a0
add t3, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
add a0, t2, zero
add a1, t3, zero
call gcd
ld a1, 0(sp)
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
add t4, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t4, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
add a0, t4, zero
call id
ld a1, 0(sp)
addi sp, sp, 8
ld t3, 0(sp)
addi sp, sp, 8
ld t4, 0(sp)
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
add t2, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
add a0, a1, zero
call id
ld a1, 0(sp)
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
lcm:
mul t0, a0, a1
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
add a1, a1, zero
call gcd
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
div t2, t0, a0
add a0, t2, zero
ret
findDiv:
beq a1, a2, l2
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
addi sp, sp, -8
sd a2, 0(sp)
add a0, t1, zero
add a1, a1, zero
call lcm
ld a2, 0(sp)
addi sp, sp, 8
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
addi t2, zero, 1
add t3, a1, t2
add t4, a0, zero
addi sp, sp, -8
sd t4, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
addi sp, sp, -8
sd a2, 0(sp)
add a0, t4, zero
add a1, t3, zero
add a2, a2, zero
call findDiv
ld a2, 0(sp)
addi sp, sp, 8
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld t3, 0(sp)
addi sp, sp, 8
ld t2, 0(sp)
addi sp, sp, 8
ld t4, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t5, a0, zero
addi sp, sp, -8
sd t4, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
addi sp, sp, -8
sd t5, 0(sp)
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
addi sp, sp, -8
sd a2, 0(sp)
add a0, t5, zero
call id
ld a2, 0(sp)
addi sp, sp, 8
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld t5, 0(sp)
addi sp, sp, 8
ld t3, 0(sp)
addi sp, sp, 8
ld t2, 0(sp)
addi sp, sp, 8
ld t4, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t0, a0, zero
j l3
l2:
addi sp, sp, -8
sd ra, 0(sp)
add t1, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd a1, 0(sp)
addi sp, sp, -8
sd a2, 0(sp)
add a0, t1, zero
call id
ld a2, 0(sp)
addi sp, sp, 8
ld a1, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t0, a0, zero
l3:
add a0, t0, zero
ret
main:
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd ra, 0(sp)
addi t0, zero, 1
addi t1, zero, 1
addi t2, zero, 21
add t3, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
add a0, t0, zero
add a1, t1, zero
add a2, t2, zero
call findDiv
ld t2, 0(sp)
addi sp, sp, 8
ld t1, 0(sp)
addi sp, sp, 8
ld t0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
add t3, a0, zero
addi sp, sp, -8
sd t0, 0(sp)
addi sp, sp, -8
sd t1, 0(sp)
addi sp, sp, -8
sd t2, 0(sp)
addi sp, sp, -8
sd t3, 0(sp)
add a0, t3, zero
call outputInt
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