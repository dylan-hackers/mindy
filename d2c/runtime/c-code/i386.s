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

        pushl   12(%ebp)
        call    8(%ebp)

        leal    4(%esp), %esp

restore_state:
        popl    %edi
        popl    %esi
        popl    %ebx
        leave
        ret

.globl  _restore_state
_restore_state:
        pushl   %ebp
        movl    %esp, %ebp

        movl    8(%ebp), %esp
        movl    12(%ebp), %eax
        jmp     restore_state
