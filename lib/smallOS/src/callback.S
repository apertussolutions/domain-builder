# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND


        # This is called by the hypervisor when an event occurs.
        # NOTE: We assume that we are executing on VCPU 0.

        .global hypervisor_callback
hypervisor_callback:
        subq $(14 * 8), %rsp          # make register structure
        # an extra slot to ensure 16-byte stack alignment

        # preserved across calls
        movq %r15,  1 * 8 (%rsp)
        movq %r14,  2 * 8 (%rsp)
        movq %r13,  3 * 8 (%rsp)
        movq %r12,  4 * 8 (%rsp)
        movq %rbp,  5 * 8 (%rsp)
        movq %rbx,  6 * 8 (%rsp)

        # not preserved across calls
        movq %r10,  7 * 8 (%rsp)
        movq %r9,   8 * 8 (%rsp)
        movq %r8,   9 * 8 (%rsp)
        movq %rax, 10 * 8 (%rsp)
        movq %rdx, 11 * 8 (%rsp)
        movq %rsi, 12 * 8 (%rsp)
        movq %rdi, 13 * 8 (%rsp)

1:      leaq 8(%rsp), %rdi      # first argument is pointer to structure
                                # skipping empty alignment slot

        call do_hypervisor_callback

        # Done with this pass, now enter critical section
        movb $0,     HYPERVISOR_shared_info + 1     # clear upcall_mask

# ----- BEGIN: Critical section ------------------------------------------------
#
#       While we are executing the instructions bellow, we may be intrrupted
#       by another upcall to handle more events.
#
#       If this happens a lot, then we could---potentially---use up all
#       the stack.  The HaLVM has a fixup that, perhaps, we should use?

        testb $0xFF, HYPERVISOR_shared_info         # check pending
        jnz 2f                                      # if yes, go again

        # We are done, for now.
        movq  1 * 8 (%rsp), %r15
        movq  2 * 8 (%rsp), %r14
        movq  3 * 8 (%rsp), %r13
        movq  4 * 8 (%rsp), %r12
        movq  5 * 8 (%rsp), %rbp
        movq  6 * 8 (%rsp), %rbx

        movq  7 * 8 (%rsp), %r10
        movq  8 * 8 (%rsp), %r9
        movq  9 * 8 (%rsp), %r8
        movq 10 * 8 (%rsp), %rax
        movq 11 * 8 (%rsp), %rdx
        movq 12 * 8 (%rsp), %rsi
        movq 13 * 8 (%rsp), %rdi
        movq 14 * 8 (%rsp), %rcx
        movq 15 * 8 (%rsp), %r11

        addq $(16 * 8), %rsp

        andl  $~0x80000000, 2 * 8 (%rsp)   # clear NMI_MASK in EFLAGS
        pushq $0
        jmp  HYPERVISOR_iret

2:      movb $1, HYPERVISOR_shared_info + 1     # set upcall_mask

# ----- END: Critical section --------------------------------------------------

        jmp 1b


