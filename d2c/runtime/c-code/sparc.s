# 1 "/afs/cs/project/gwydion//dylan/src/d2c/runtime/c-code/sparc.S"

# 1 "/usr/include/sys/stack.h" 1 3
 






#pragma ident	"@(#)stack.h	1.8	95/02/08 SMI"
 











 


















 









 





# 86 "/usr/include/sys/stack.h" 3


# 97 "/usr/include/sys/stack.h" 3







# 2 "/afs/cs/project/gwydion//dylan/src/d2c/runtime/c-code/sparc.S" 2

# 1 "/usr/include/sys/trap.h" 1 3
 






#pragma ident	"@(#)trap.h	1.26	94/11/22 SMI"






 






 












 












 






















# 3 "/afs/cs/project/gwydion//dylan/src/d2c/runtime/c-code/sparc.S" 2


	.global	call_on_stack
call_on_stack:
	call	%o0
	sub	%o1, ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) , %sp
	unimp	0

	.global	save_state
save_state:
	save	%sp, -(((( 8*4 )+(8 -1)) & ~(8 -1)) + ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) ), %sp
	ta	0x03 
	st	%i7, [%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) ]
	st	%g1, [%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +4]
	std	%g2, [%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +8]
	std	%g4, [%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +16]
	std	%g6, [%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +24]
	! ### Should also save the FP state.
	mov	%i1, %o1
	call	%i0
	mov	%sp, %o0
	mov	%o0, %i0
_restore_state:
	ld	[%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +4], %g1
	ldd	[%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +8], %g2
	ldd	[%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +16], %g4
	ldd	[%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) +24], %g6
	ret
	restore

	.global	restore_state
restore_state:
	ta	0x03 
	mov	%o0, %fp
	mov	%o1, %i0
	restore
	ld	[%sp+ ((( ((16*4) + (6*4) +4)  )+(8 -1)) & ~(8 -1)) ], %i7
	b _restore_state
	mov	%o0, %i0
