
#ifdef MACH
#include <machine/regdef.h>
#define s8 $30
#endif

#ifdef sgi
#include <sys/regdef.h>
#endif

	.text
	.align	2


	.globl	call_on_stack
	.ent	call_on_stack
call_on_stack:
	/* a0 = function to call */
	/* a1 = arg to pass it */
	/* a2 = stack base */
	/* a3 = stack top */
	subu	sp, a3, 16
	move	t0, a0
	move	a0, a1
	move	ra, zero
	j	a0
	.end	call_on_stack


	.globl	save_state
	.ent	save_state
save_state:
	/* a0 = function to call */
	/* a1 = additional arg to pass it */
	subu	sp, 40
	.frame	sp, 40, ra
	/* Save all the C regs. */
	.mask	0xc0ff0000, 0
	sw	ra, 40(sp)
	sw	s8, 40-4(sp)
	sw	s7, 40-8(sp)
	sw	s6, 40-12(sp)
	sw	s5, 40-16(sp)
	sw	s4, 40-20(sp)
	sw	s3, 40-24(sp)
	sw	s2, 40-28(sp)
	sw	s1, 40-32(sp)
	sw	s0, 40-36(sp)

	/* ### Should also save the floating point state. */

	move	t0, a0
	/* a1 is already set up. */
	move	a0, sp

	jal	t0

_restore_state:

	lw	ra, 40(sp)
	lw	s8, 40-4(sp)
	lw	s7, 40-8(sp)
	lw	s6, 40-12(sp)
	lw	s5, 40-16(sp)
	lw	s4, 40-20(sp)
	lw	s3, 40-24(sp)
	lw	s2, 40-28(sp)
	lw	s1, 40-32(sp)
	lw	s0, 40-36(sp)

	addu	sp, 40
	j	ra
	.end	save_state

	.globl	restore_state
	.ent	restore_state
restore_state:
	/* a0 - saved state */
	/* a1 - value to make save_state return */
	move	sp, a0
	move	v0, a1
	j	_restore_state
	.end	restore_state


	.globl	enumerate_stack
	.ent	enumerate_stack
enumerate_stack:
	/* a0 = function to call */
	/* a1 = saved state */
	/* a2 = stack base */
	/* a3 = stack top */
	move	t0, a0
	move	a0, a1
	sub	a1, a3, a1
	j	t0
	.end	enumerate_stack
