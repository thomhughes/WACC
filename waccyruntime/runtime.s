	.cpu arm1176jzf-s
	.arch armv6kz
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"runtime.c"
	.text
	.section	.rodata
	.align	2
.LC0:
	.ascii	"Runtime error: Unknown runtime error, array of elem"
	.ascii	"ent size: %d\012\000"
	.text
	.align	2
	.global	array_literal_create
	.syntax unified
	.arm
	.type	array_literal_create, %function
array_literal_create:
	@ args = 4, pretend = 4, frame = 32
	@ frame_needed = 1, uses_anonymous_args = 1
	str	r3, [sp, #-4]!
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #36
	str	r0, [fp, #-28]
	str	r1, [fp, #-32]
	str	r2, [fp, #-36]
	ldr	r3, [fp, #4]
	str	r3, [fp, #-40]
	ldr	r3, .L7
	ldr	r3, [r3]
	str	r3, [fp, #-12]
	mov	r3, #0
	add	r3, fp, #8
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-28]
	ldr	r2, [fp, #-32]
	mul	r3, r2, r3
	add	r3, r3, #8
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	ldr	r2, [fp, #-28]
	str	r2, [r3]
	ldr	r3, [fp, #-16]
	add	r3, r3, #4
	add	r2, r3, #16
	ldr	r3, [fp, #-16]
	str	r2, [r3, #4]
	ldr	r3, [fp, #-28]
	cmp	r3, #1
	bne	.L2
	ldr	r3, [fp, #-16]
	ldr	r3, [r3, #4]
	ldr	r2, [fp, #-20]
	ldr	r1, [fp, #-32]
	mov	r0, r3
	bl	array_literal_create_bytes
	b	.L3
.L2:
	ldr	r3, [fp, #-28]
	cmp	r3, #4
	bne	.L4
	ldr	r3, [fp, #-16]
	ldr	r3, [r3, #4]
	ldr	r2, [fp, #-20]
	ldr	r1, [fp, #-32]
	mov	r0, r3
	bl	array_literal_create_longs
	b	.L3
.L4:
	ldr	r3, .L7+4
	ldr	r3, [r3]
	ldr	r2, [fp, #-28]
	ldr	r1, .L7+8
	mov	r0, r3
	bl	fprintf
.L3:
	ldr	r3, [fp, #-16]
	ldr	r2, .L7
	ldr	r1, [r2]
	ldr	r2, [fp, #-12]
	eors	r1, r2, r1
	mov	r2, #0
	beq	.L6
	bl	__stack_chk_fail
.L6:
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, lr}
	add	sp, sp, #4
	bx	lr
.L8:
	.align	2
.L7:
	.word	__stack_chk_guard
	.word	stderr
	.word	.LC0
	.size	array_literal_create, .-array_literal_create
	.align	2
	.global	array_size
	.syntax unified
	.arm
	.type	array_size, %function
array_size:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	ldr	r3, [r3]
	mov	r0, r3
	add	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	array_size, .-array_size
	.align	2
	.global	array_access
	.syntax unified
	.arm
	.type	array_access, %function
array_access:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	ldr	r1, [fp, #-12]
	ldr	r0, [fp, #-8]
	bl	check_array_access
	ldr	r3, [fp, #-8]
	ldr	r2, [r3, #4]
	ldr	r3, [fp, #-12]
	add	r3, r2, r3
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	array_access, .-array_access
	.align	2
	.global	pair_create
	.syntax unified
	.arm
	.type	pair_create, %function
pair_create:
	@ args = 8, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #24
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	str	r3, [fp, #-28]
	mov	r0, #8
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #4]
	str	r2, [r3]
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #8]
	str	r2, [r3, #4]
	ldr	r3, [fp, #-8]
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	pair_create, .-pair_create
	.align	2
	.global	pair_fst
	.syntax unified
	.arm
	.type	pair_fst, %function
pair_fst:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	str	r2, [fp, #-16]
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-8]
	mov	r0, r3
	add	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	pair_fst, .-pair_fst
	.align	2
	.global	pair_snd
	.syntax unified
	.arm
	.type	pair_snd, %function
pair_snd:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-8]
	str	r1, [fp, #-12]
	str	r2, [fp, #-16]
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-8]
	add	r3, r3, #4
	mov	r0, r3
	add	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	pair_snd, .-pair_snd
	.ident	"GCC: (Ubuntu 11.3.0-1ubuntu1~22.04) 11.3.0"
	.section	.note.GNU-stack,"",%progbits
