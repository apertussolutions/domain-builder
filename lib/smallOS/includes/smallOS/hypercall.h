//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_HYPERCALL_H__
#ifndef S_SPLINT_S
#define __SMALLOS_HYPERCALL_H__

#include <smallOS/xen-version.h>
#include <xen/xen.h>
#include <xen/sched.h>
#include <xen/physdev.h>
#include <xen/xsm/flask_op.h>

// 1
extern int  HYPERVISOR_set_trap_table(trap_info_t *table);
extern long HYPERVISOR_set_timer_op(uint64_t timeout);
extern int  HYPERVISOR_fpu_taskswitch(int set);
extern long HYPERVISOR_get_debugreg(int reg);
extern int  HYPERVISOR_physdev_op(void *physdev_op);
extern int  HYPERVISOR_sysctl(unsigned long op);
extern int  HYPERVISOR_domctl(void* op);
extern int  HYPERVISOR_xsm_op(xen_flask_op_t *op);

// 2
extern int  HYPERVISOR_set_gdt(unsigned long *frame_list, int entries);
extern int  HYPERVISOR_stack_switch(unsigned long ss, unsigned long esp);
extern int  HYPERVISOR_sched_op(int cmd, /*@null@*/ void *arg);
extern int  HYPERVISOR_hvm_op(int cmd, /*@null@*/ void *arg);
extern int  HYPERVISOR_set_debugreg(int reg, unsigned long value);
extern int  HYPERVISOR_update_descriptor(unsigned long ma, unsigned long word);
extern int  HYPERVISOR_memory_op(unsigned int cmd, void *arg);
extern int  HYPERVISOR_multicall(void *call_list, int nr_calls);
extern int  HYPERVISOR_event_channel_op(int cmd, void *op);
extern int  HYPERVISOR_xen_version(int cmd, void *arg);
extern int  HYPERVISOR_vm_assist(unsigned int cmd, unsigned int type);

extern int  HYPERVISOR_set_segment_base(int reg, unsigned long value);

extern int  HYPERVISOR_nmi_op(unsigned long op, unsigned long arg);

// 3
extern int  HYPERVISOR_set_callbacks
              (void *event, void *failsafe, void *syscall);

extern int  HYPERVISOR_update_va_mapping
              (unsigned long va, uint64_t new_val, unsigned long flags);
extern int  HYPERVISOR_console_io(int cmd, int count, char *str);
extern int  HYPERVISOR_grant_table_op
              (unsigned int cmd, void *uop, unsigned int count);
extern int  HYPERVISOR_vcpu_op(int cmd, int vcpuid, void *extra_args);



// 4
extern int HYPERVISOR_mmu_update
  (mmu_update_t *req, int count, /*@null@*/ int * success_count, domid_t domid);

extern int  HYPERVISOR_mmuext_op
  (struct mmuext_op *op, int count, /*@null@*/ int *success_count, domid_t domid);

extern int  HYPERVISOR_update_va_mapping_otherdomain
  (unsigned long va, uint64_t new_val, unsigned long flags, domid_t domid);


#endif /* S_SPLINT_S */
#endif

