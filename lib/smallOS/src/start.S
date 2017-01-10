# BANNERSTART
# Copyright: 2011, Galois, Inc.
# License-file: LICENSE
# Author: Iavor S. Diatchki (diatchki@galois.com)
# BANNEREND

#define __ASSEMBLY__
#define __XEN_INTERFACE_VERSION__ __XEN_LATEST_INTERFACE_VERSION__

#include <xen/xen.h>
#include <xen/elfnote.h>

# -- Xen notes -----------------------------------------------------------------
        .section __xen_guest,"","note"

#define NOTE(type, desctype, descdata)   \
        .balign 4                       ;\
        .long 2f - 1f   /* namesz */    ;\
        .long 4f - 3f   /* descsz */    ;\
        .long  type                     ;\
1:      .asciz "Xen"                    ;\
2:      .balign 4                       ;\
3:      desctype descdata               ;\
4:      .balign 4


        NOTE (XEN_ELFNOTE_GUEST_OS,        .asciz,   "SmallVM")
        NOTE (XEN_ELFNOTE_XEN_VERSION,     .asciz,   "xen-3.0")
        NOTE (XEN_ELFNOTE_VIRT_BASE,       .long,    0x1000)
        NOTE (XEN_ELFNOTE_PADDR_OFFSET,    .long,    0x1000)
        NOTE (XEN_ELFNOTE_ENTRY,           .long,    _start)
        NOTE (XEN_ELFNOTE_HYPERCALL_PAGE,  .long,    hypercall_page)
        NOTE (XEN_ELFNOTE_LOADER,          .asciz,   "generic")


        # Layout of the kernel:
        #
        # page:    0      | 1          | 2
        # content: unused | hypercalls | kernel code


# ------------------------------------------------------------------------------


# -- Hypercalls ----------------------------------------------------------------

        # Here we define Xen's hypercalls.
        # (using two preprocessors!)


        .text
        .balign 0x1000
hypercall_page:
        .space 0x1000               # skip the hyper call page

#define HC(x) HypercallEntry x, __HYPERVISOR_##x

        .macro HypercallEntry name, num
        .global HYPERVISOR_\name
        .set HYPERVISOR_\name, hypercall_page + 32 * \num
        .endm

        HC(set_trap_table)
        HC(set_gdt)
        HC(stack_switch)
        HC(set_callbacks)
        HC(fpu_taskswitch)
        HC(sched_op)
        HC(set_timer_op)
        HC(set_debugreg)
        HC(get_debugreg)
        HC(update_descriptor)
        HC(memory_op)
        HC(multicall)
        HC(event_channel_op)
        HC(xen_version)
        HC(console_io)
        HC(physdev_op)
        HC(hvm_op)
        HC(grant_table_op)
        HC(update_va_mapping)
        HC(vm_assist)
        HC(vcpu_op)
        HC(set_segment_base)
        HC(nmi_op)
        HC(sysctl)
        HC(domctl)
        HC(iret)

        # XSM
        HC(xsm_op)

# -- Kernel entry --------------------------------------------------------------

        .bss
        .global _stack_unmap, _stack_end, _stack

        .balign 0x1000
_stack_unmap:
        .space 0x1000         # unmap, to notice stack overflows
_stack_end:
        .space 512 * 1024     # 512K of run-time stack
_stack:

        # Fill a (properly aligend) region of 64-bit words
        # with the value in %rax.
        .macro zero64, from, to
        movq $\from,  %rdi                    # starting herej
        movq $\to,    %rcx                    # rcx = (to - from) / 8
        subq %rdi,    %rcx                    # ...
        shrq $3,      %rcx                    # ...
        rep stosq                             # go!
        .endm

        .section noreset,"aw"
_reset_counter:
        .quad 1


        .text
        .global _start, _reset_start
_start: cld
        leaq _stack,       %rsp   # setup stack (stack defined externally)
        movq %rsi,         %rbx   # save start info for a bit

        # Unmap last page of the stack to detect potential overflows.
        leaq _stack_unmap, %rdi
        movq $0,           %rsi   # New entry (i.e., unmap)
        movq $2,           %rdx   # UVMF_INVLPG, invalidate just this entry
        movq $HYPERVISOR_update_va_mapping, %rax
        call *%rax

        # Initialize kernel's persistent state.
        movq $0,           _reset_counter
        movq %rbx,         %rdi   # pass start info as argument
        call kernel_init

        # Enter the resetable kernel loop.
_reset_start:
        incq _reset_counter
        call reset_virtual_region             # Reset: foreign mapped pages.
        xorq %rax, %rax                       # Fill with 0s
        zero64 _zero_start, _stack_unmap      # Reset: zero_start--stack-unmap
        zero64 _stack_end, _zero_end          # Reset: stack_end--stack
        xorq %rax, %rax                       # Reset: registers
        xorq %rbx, %rbx                       # ...
        xorq %rcx, %rcx                       # ...
        xorq %rdx, %rdx                       # ...
        xorq %rsi, %rsi                       # ...
        xorq %rdi, %rdi                       # ...
        xorq %rsi, %rsi                       # ...
        xorq %rbp, %rbp                       # ...
        xorq %r8,  %r8                        # ...
        xorq %r9,  %r9                        # ...
        xorq %r10, %r10                       # ...
        xorq %r11, %r11                       # ...
        xorq %r12, %r12                       # ...
        xorq %r13, %r13                       # ...
        xorq %r14, %r14                       # ...
        xorq %r15, %r15                       # ...
        leaq _stack, %rsp                     # ...
        push $_reset_start                    # Return to the top of the loop.
        jmp kernel_loop                       # Go kernel!


# -- (XXX) 4 argument hypercalls -----------------------------------------------

        # Normally, the 4th argument is passed in %rcx but
        # Xen expects it in $r10, so we adjust things here
        # (may be slowish...)

        # XXX: We could avoid this by using a bit of
        # inline asm but there seems to be some sort of a
        # weird interaction between gcc -O and asm
        # (gcc 4.4.1)

        .macro HypercallEntry4 name, num
        .global HYPERVISOR_\name
HYPERVISOR_\name:
        movq %rcx, %r10
        jmp hypercall_page + 32 * \num
        .endm

#define HC4(x) HypercallEntry4 x, __HYPERVISOR_##x

        HC4(mmu_update)
        HC4(mmuext_op)
        HC4(update_va_mapping_otherdomain)


