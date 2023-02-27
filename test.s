.text
.global main
main:
	push {fp, lr}
.data
	.word 5
.L.str0:
	.asciz "Hello"
.text
	ldr r8, =.L.str0
	push {r8}
	pop {r0}
	bl _prints
	pop {fp, pc}
.data
	.word 4
.L._prints_str0:
	.asciz "%.*s"
.text
_prints:
	push {lr}
	mov r2, r0
	ldr r1, [r0, #-4]
	ldr r0, =.L._prints_str0
	bl printf
	mov r0, #0
	bl fflush
	pop {pc}
