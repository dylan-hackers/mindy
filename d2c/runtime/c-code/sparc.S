#define _ASM
#include <sys/stack.h>
#include <sys/trap.h>

	.global	call_on_stack
call_on_stack:
	call	%o0
	sub	%o1, SA(MINFRAME), %sp
	unimp	0

	.global	save_state
save_state:
	save	%sp, -(SA(8*4)+SA(MINFRAME)), %sp
	ta	ST_FLUSH_WINDOWS
	st	%i7, [%sp+SA(MINFRAME)]
	st	%g1, [%sp+SA(MINFRAME)+4]
	std	%g2, [%sp+SA(MINFRAME)+8]
	std	%g4, [%sp+SA(MINFRAME)+16]
	std	%g6, [%sp+SA(MINFRAME)+24]
	! ### Should also save the FP state.
	mov	%i1, %o1
	call	%i0
	mov	%sp, %o0
	mov	%o0, %i0
_restore_state:
	ld	[%sp+SA(MINFRAME)+4], %g1
	ldd	[%sp+SA(MINFRAME)+8], %g2
	ldd	[%sp+SA(MINFRAME)+16], %g4
	ldd	[%sp+SA(MINFRAME)+24], %g6
	ret
	restore

	.global	restore_state
restore_state:
	ta	ST_FLUSH_WINDOWS
	mov	%o0, %fp
	mov	%o1, %i0
	restore
	ld	[%sp+SA(MINFRAME)], %i7
	b _restore_state
	mov	%o0, %i0
