    .data
	.text
	.global main
	main:
		push {fp, lr}
		push {r8, r10, r12}
		mov fp, sp
		ldr r8, =-1
		mov r8, r8		
        mov r0, r8
		@ statement primitives do not return results (but will clobber r0/rax)
		bl exit
		mov r0, #0
		pop {r8, r10, r12}
		pop {fp, pc}