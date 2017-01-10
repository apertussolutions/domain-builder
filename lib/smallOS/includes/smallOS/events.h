//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_EVENTS_H__
#define __SMALLOS_EVENTS_H__

#include <smallOS/xen-version.h>
#include <smallOS/hypercall.h>
#include <xen/xen.h>

// NOTE: Initializes shared-info page and other things.
int init_events(start_info_t *si);

static inline
int block(void) { return HYPERVISOR_sched_op(SCHEDOP_block, 0); }


#endif
