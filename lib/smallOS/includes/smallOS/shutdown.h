//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_SHUTDOWN_H__
#ifndef S_SPLINT_S
#define __SMALLOS_SHUTDOWN_H__

#include <smallOS/xen-version.h>
#include <smallOS/hypercall.h>
#include <xen/sched.h>

__attribute__((noreturn))
static inline
void shutdown(unsigned int reason) {
  sched_shutdown_t op = { .reason = reason };
  HYPERVISOR_sched_op(SCHEDOP_shutdown, &op);
  asm("ud2");
  while(1);
}

#endif /* S_SPLINT_S */
#endif
