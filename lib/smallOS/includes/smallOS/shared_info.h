//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_SHARED_INFO_H__
#define __SMALLOS_SHARED_INFO_H__

#include <smallOS/evtchn.h>
#include <smallOS/mem.h>
#include <smallOS/utils.h>

extern volatile
shared_info_t HYPERVISOR_shared_info __attribute__ ((aligned(PAGE_SIZE))) ;



// Map the shared info page into virtual space, and mask out all events.
int init_shared_info(start_info_t *si);

static inline
void clear_evtchn(evtchn_port_t port) {
  synch_clear_bit(port, &HYPERVISOR_shared_info.evtchn_pending);
}

static inline
int check_evtchn(evtchn_port_t port) {
  return synch_test_bit(port, &HYPERVISOR_shared_info.evtchn_pending);
}

static inline
void mask_evtchn(evtchn_port_t port) {
  synch_set_bit(port, &HYPERVISOR_shared_info.evtchn_mask);
}

static inline
void unmask_evtchn(evtchn_port_t port) {
  synch_clear_bit(port, &HYPERVISOR_shared_info.evtchn_mask);
}

#endif


