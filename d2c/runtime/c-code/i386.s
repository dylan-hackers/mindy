
# call_on_stack(fn, arg, stack_base, stack_end)
#
# call fn(arg) using stack_base...stack_end as the stack.
# fn is not allowed to return.
#
.globl  _call_on_stack
_call_on_stack:

	# Build our frame.  Doing it by hand is faster than using enter.
        pushl   %ebp
        movl    %esp, %ebp

	# set up the new stack by moving stack_end into esp
        movl    20(%ebp), %esp

	# call fn(arg)
	pushl	12(%ebp)
        call    8(%ebp)

	# puke if it returns.
        hlt

# save_state(callback, arg)
#
# Save whatever state we need to make restore_state work and then call
# callback(state_ptr, arg).
#
.globl  _save_state
_save_state:

	# Build our frame.  Doing it by hand is faster than using enter.
        pushl   %ebp
        movl    %esp, %ebp

	# save the callee saves registers.
        pushl   %ebx
        pushl   %esi
        pushl   %edi

	# snapshot the stack pointer at just below the saved state.
	movl	%esp, %eax

	# do the call.
        pushl   12(%ebp)
	pushl	%eax
        call    8(%ebp)

	# clear the two pushed args.
        leal    8(%esp), %esp

	# Leave %eax alone.  It holds the return value, and we're just
	# going to use the return value from the function we called.

restore_state:

	# restore the saved state.
        popl    %edi
        popl    %esi
        popl    %ebx

	# we don't use a leave, because the current ebp is trash and
	# besides, we don't need to.
	popl	%ebp

	# and back we go.
        ret

# restore_state(saved_state_ptr, return_value)
#
# Restore the state saved at saved_state_ptr and make the call to save_state
# that saved that state immediately return with return_value.
#
.globl  _restore_state
_restore_state:

	# Build our frame.  Doing it by hand is faster than using enter.
        pushl   %ebp
        movl    %esp, %ebp

	# unwind the stack to the saved state.
        movl    8(%ebp), %esp

	# get the return value
        movl    12(%ebp), %eax

	# jump into the tail of save_state in order to do the actual restore
	# and return.
        jmp     restore_state
