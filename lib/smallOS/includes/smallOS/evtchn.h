//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_EVTCHN_H__
#define __SMALLOS_EVTCHN_H__

#include <smallOS/xen-version.h>
#include <smallOS/hypercall.h>
#include <xen/xen.h>
#include <xen/event_channel.h>
#include <xen/sched.h>

int bind_virq(uint32_t virq, evtchn_port_t *port);
int bind_pirq(uint32_t pirq, int will_share, evtchn_port_t *port);
int bind_unbound_from_to(domid_t from, domid_t to, evtchn_port_t *port);
int bind_unbound(domid_t pal, evtchn_port_t *port);
int bind_interdomain(domid_t pal, evtchn_port_t remote_port,
          evtchn_port_t *local_port);


int unbind(evtchn_port_t port);

int send_event(evtchn_port_t port);
int receive_event(uint64_t timeout,
                            unsigned long size, evtchn_port_t ports[]);
static inline
int yield(void) { return HYPERVISOR_sched_op(SCHEDOP_yield, 0); }

#endif
