
	.space	$TEXT$
	.subspa	$CODE$

	.import $$dyncall,MILLICODE

	.export call_on_stack
call_on_stack
	; arg0 = fn to invoke
	; arg1 = arg to pass it
	; arg2 = stack base
	; arg3 = stack end
	.proc
	.callinfo calls
	.entry

	; Compute the new stack pointer.
	addi	64,%arg2,%sp

	; Zero out the previous stack pointer.
	stw	%r0,-4(0,%sp)

	; Copy the function into the place dyncall wants it.
	copy	%arg0, %r22

	; Move the arg into the correct place.
	copy	%arg1, %arg0

	; Invoke the function.
	bl	$$dyncall, %r31
	copy	%r31, %r2

	; Flame out if it ever returns.
	.exit
	break	0,0
	.procend


	.export	save_state
save_state
	; arg0 = function to call once we have saved our state
	; arg1 = arg to pass it
	.proc
	.callinfo entry_gr=18,entry_fr=21,entry_sr=3,save_rp,calls
	.enter

	; Copy the function into the place dyncall wants it.
	copy	%arg0,%r22

	; Pass the new stack pointer in as arg0, and leave arg1 as arg1.
	copy	%sp,%arg0

	; do the call.
	bl	$$dyncall, %r31
	copy	%r31, %r2

_restore_state
	.leave
	.procend

	.export	restore_state
restore_state
	; arg0 = stack point to restore
	; arg1 = value to make save_state return
	.proc
	.callinfo
	.entry


	copy	%arg0,%sp
	b	_restore_state
	copy	%arg1,%ret0
	.procend


	.export	enumerate_stack
enumerate_stack
	; arg0 = fn to call
	; arg1 = machine state (i.e. stack top)
	; arg2 = stack base
	; arg3 = stack end
	.proc
	.callinfo calls,save_rp
	.enter

	; move the function to where dyncall wants it.
	copy	%arg0,%r22

	; start at the stack base.
	copy	%arg2,%arg0

	; compute the length of the active stack.
	sub	%arg1,%arg2,%arg1

	; call the function.
	bl	$$dyncall,%r31
	copy	%r31,%r2

	; back we go.
	.leave
	.procend

	.end
