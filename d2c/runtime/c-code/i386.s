.globl  _call_on_stack
_call_on_stack:
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %esp
        call    12(%ebp)
        hlt

.globl  _save_state
_save_state:
        pushl   %ebp
        movl    %esp, %ebp

        pushl   %ebx
        pushl   %esi
        pushl   %edi

	movl	%esp, %eax
        pushl   12(%ebp)	# Get info argument + push on stack
	pushl	%eax		# it gets the machine state argument 
				# + pushs on stack
        call    8(%ebp)         # 8(%ebp) is the function pointer

        leal    8(%esp), %esp

restore_state:
        popl    %edi
        popl    %esi
        popl    %ebx
	popl	%ebp
	#	        leave
        ret

.globl  _restore_state
_restore_state:
        pushl   %ebp
        movl    %esp, %ebp

        movl    8(%ebp), %esp
        movl    12(%ebp), %eax
        jmp     restore_state
