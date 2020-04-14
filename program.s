	.arch armv7-a
	.file	"program.s"
	.global	__aeabi_idiv
	.section	.rodata
	.align	2
L4:
	.long	2
	.ascii	" O"
	.align	2
L5:
	.long	2
	.ascii	" ."
	.align	2
L12:
	.long	1
	.ascii	"\n"
	.align	2
L16:
	.long	1
	.ascii	"\n"
	.text

	@prologo:
.align 2
.global L0
.type   L0, %function
	L0:
	push {r4,r5,r6,r7,r8,r9,r10,fp,lr}
	sub     fp, sp, #4
	sub     sp, #4

L29:
str     r0, [fp, #0]
ldr     r10, [fp, #0]
ldr     r10, [r10, #-4]
mov r1, #1
sub     r5, r10, r1
movw   r10, #:lower16:0
movt   r10, #:upper16:0
L13:
cmp     r10, r5
ble L14
L2:
movw    r0, #:lower16:L16
movt    r0, #:upper16:L16
bl      print
b       L28
L15:
mov r4, #1
add     r10, r10, r4
b       L13
L14:
ldr     r4, [fp, #0]
ldr     r4, [r4, #-4]
mov r3, #1
sub     r7, r4, r3
movw   r6, #:lower16:0
movt   r6, #:upper16:0
L9:
cmp     r6, r7
ble L10
L3:
movw    r0, #:lower16:L12
movt    r0, #:upper16:L12
bl      print
cmp     r10, r5
beq L2
L30:
b       L15
L11:
mov r4, #1
add     r6, r6, r4
b       L9
L10:
ldr     r4, [fp, #0]
ldr     r4, [r4, #-12]
mov     r0, r4
mov     r1, r10
bl      _checkIndexArray
mov r3, #2
lsl     r3, r10, r3
add     r4, r4, r3
ldr    r4, [r4]
cmp     r4, r6
beq L6
L7:
movw    r0, #:lower16:L5
movt    r0, #:upper16:L5
L8:
bl      print
cmp     r6, r7
beq L3
L31:
b       L11
L6:
movw    r0, #:lower16:L4
movt    r0, #:upper16:L4
b       L8
L28:

@epilogo
	add     sp, #4
	pop {r4,r5,r6,r7,r8,r9,r10,fp,pc}

	@prologo:
.align 2
.global L1
.type   L1, %function
	L1:
	push {r4,r5,r6,r7,r8,r9,r10,fp,lr}
	sub     fp, sp, #4
	sub     sp, #4

L33:
str     r0, [fp, #0]
mov     r10, r1
ldr     r1, [fp, #0]
ldr     r1, [r1, #-4]
cmp     r10, r1
beq L25
L26:
ldr     r1, [fp, #0]
ldr     r1, [r1, #-4]
mov r8, #1
sub     r4, r1, r8
movw   r8, #:lower16:0
movt   r8, #:upper16:0
L22:
cmp     r8, r4
ble L23
L17:
L27:
b       L32
L25:
ldr     r0, [fp, #0]
bl      L0
b       L27
L24:
mov r6, #1
add     r8, r8, r6
b       L22
L23:
ldr     r7, [fp, #0]
ldr     r6, [r7, #-8]
mov     r0, r6
mov     r1, r8
bl      _checkIndexArray
mov r3, #2
lsl     r3, r8, r3
add     r6, r6, r3
ldr    r6, [r6]
movw   r5, #:lower16:0
movt   r5, #:upper16:0
cmp     r6, r5
beq L18
L21:
cmp     r8, r4
beq L17
L34:
b       L24
L18:
ldr     r7, [fp, #0]
ldr     r6, [r7, #-16]
add     r1, r8, r10
mov     r0, r6
bl      _checkIndexArray
add     r5, r8, r10
mov r3, #2
lsl     r5, r5, r3
add     r6, r6, r5
ldr    r6, [r6]
movw   r5, #:lower16:0
movt   r5, #:upper16:0
cmp     r6, r5
bne L21
L19:
ldr     r7, [fp, #0]
ldr     r6, [r7, #-20]
mov r7, #7
add     r7, r8, r7
sub     r1, r7, r10
mov     r0, r6
bl      _checkIndexArray
mov r5, #7
add     r5, r8, r5
sub     r5, r5, r10
mov r2, #2
lsl     r5, r5, r2
add     r6, r6, r5
ldr    r6, [r6]
movw   r3, #:lower16:0
movt   r3, #:upper16:0
cmp     r6, r3
bne L21
L20:
ldr     r7, [fp, #0]
ldr     r6, [r7, #-8]
mov     r0, r6
mov     r1, r8
bl      _checkIndexArray
mov r7, #1
mov r5, #2
lsl     r5, r8, r5
str     r7, [r6, r5]
ldr     r7, [fp, #0]
ldr     r6, [r7, #-16]
add     r1, r8, r10
mov     r0, r6
bl      _checkIndexArray
mov r7, #1
add     r5, r8, r10
mov r3, #2
lsl     r5, r5, r3
str     r7, [r6, r5]
ldr     r7, [fp, #0]
ldr     r6, [r7, #-20]
mov r7, #7
add     r7, r8, r7
sub     r1, r7, r10
mov     r0, r6
bl      _checkIndexArray
mov r7, #1
mov r5, #7
add     r5, r8, r5
sub     r5, r5, r10
mov r3, #2
lsl     r5, r5, r3
str     r7, [r6, r5]
ldr     r7, [fp, #0]
ldr     r6, [r7, #-12]
mov     r0, r6
mov     r1, r10
bl      _checkIndexArray
mov r7, #2
lsl     r7, r10, r7
str     r8, [r6, r7]
mov r7, #1
add     r1, r10, r7
ldr     r0, [fp, #0]
bl      L1
ldr     r7, [fp, #0]
ldr     r6, [r7, #-8]
mov     r0, r6
mov     r1, r8
bl      _checkIndexArray
movw   r7, #:lower16:0
movt   r7, #:upper16:0
mov r5, #2
lsl     r5, r8, r5
str     r7, [r6, r5]
ldr     r7, [fp, #0]
ldr     r6, [r7, #-16]
add     r1, r8, r10
mov     r0, r6
bl      _checkIndexArray
movw   r7, #:lower16:0
movt   r7, #:upper16:0
add     r5, r8, r10
mov r3, #2
lsl     r5, r5, r3
str     r7, [r6, r5]
ldr     r7, [fp, #0]
ldr     r6, [r7, #-20]
mov r7, #7
add     r7, r8, r7
sub     r1, r7, r10
mov     r0, r6
bl      _checkIndexArray
movw   r5, #:lower16:0
movt   r5, #:upper16:0
mov r3, #7
add     r3, r8, r3
sub     r3, r3, r10
mov r2, #2
lsl     r3, r3, r2
str     r5, [r6, r3]
b       L21
L32:

@epilogo
	add     sp, #4
	pop {r4,r5,r6,r7,r8,r9,r10,fp,pc}

	@prologo:
.align 2
.global tigermain
.type   tigermain, %function
	tigermain:
	push {r4,r5,r6,r7,r8,r9,r10,fp,lr}
	sub     fp, sp, #4
	sub     sp, #24

L36:
str     r0, [fp, #0]
mov r10, #8
str     r10, [fp, #-4]
mov r10, #8
sub     r10, fp, r10
ldr     r0, [fp, #-4]
movw   r1, #:lower16:0
movt   r1, #:upper16:0
bl      _initArray
str     r0,[r10]
mov r10, #12
sub     r10, fp, r10
ldr     r0, [fp, #-4]
movw   r1, #:lower16:0
movt   r1, #:upper16:0
bl      _initArray
str     r0,[r10]
mov r10, #16
sub     r10, fp, r10
ldr     r1, [fp, #-4]
ldr     r0, [fp, #-4]
add     r1, r1, r0
mov r0, #1
sub     r0, r1, r0
movw   r1, #:lower16:0
movt   r1, #:upper16:0
bl      _initArray
str     r0,[r10]
mov r10, #20
sub     r10, fp, r10
ldr     r1, [fp, #-4]
ldr     r0, [fp, #-4]
add     r1, r1, r0
mov r0, #1
sub     r0, r1, r0
movw   r1, #:lower16:0
movt   r1, #:upper16:0
bl      _initArray
str     r0,[r10]
movw   r1, #:lower16:0
movt   r1, #:upper16:0
mov     r0, fp
bl      L1
b       L35
L35:

@epilogo
	add     sp, #24
	pop {r4,r5,r6,r7,r8,r9,r10,fp,pc}
