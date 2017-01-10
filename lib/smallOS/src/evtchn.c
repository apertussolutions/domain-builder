//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

// Convenience functions for making hyper-calls to manipulate event channels.

#include <smallOS/hypercall.h>
#include <smallOS/time.h>
#include <smallOS/shared_info.h>
#include <xen/event_channel.h>
#include <xen/sched.h>


// Send a notification on the given remote port.
inline
int send_event(evtchn_port_t port) {
  evtchn_send_t op = { .port = port };
  return HYPERVISOR_event_channel_op(EVTCHNOP_send, &op);
}

// The timeout is relative, in nanoseconds.
// If timeout is 0, then there is no timeout.
inline
int receive_event(uint64_t nanos, unsigned long size, evtchn_port_t ports[]) {
  sched_poll_t op;
  set_xen_guest_handle(op.ports, ports);
  op.nr_ports = size;
  op.timeout = nanos == 0 ?  0 : nanos +
                  nano_cpu_time(&HYPERVISOR_shared_info.vcpu_info[0].time);
  return HYPERVISOR_sched_op(SCHEDOP_poll, &op);
}


// Bind a virtual IRQ.
inline
int bind_virq(uint32_t virq, evtchn_port_t *port) {
  int err;
  evtchn_bind_virq_t op = { .virq = virq, .vcpu = 0 };

  err = HYPERVISOR_event_channel_op(EVTCHNOP_bind_virq, &op);
  if (err == 0) *port = op.port;
  return err;
}



// Bind a physical IRQ.
inline
int bind_pirq(uint32_t pirq, int will_share, evtchn_port_t *port) {
  int err;
  evtchn_bind_pirq_t op = { .pirq = pirq
                          , .flags = will_share ? BIND_PIRQ__WILL_SHARE : 0
                          };
  err = HYPERVISOR_event_channel_op(EVTCHNOP_bind_pirq, &op);
  if (err == 0) *port = op.port;
  return op.port;
}



// Create a new port in the "from" domain, which can be bound in the "to"
// domain to exchange messages between the two VMs.
// Requires privileges, unless "from" is "DOMID_SELF".
inline
int bind_unbound_from_to(domid_t from, domid_t to, evtchn_port_t *port) {
  int err;
  evtchn_alloc_unbound_t op = { .dom = from
                              , .remote_dom = to
                              };
  err = HYPERVISOR_event_channel_op(EVTCHNOP_alloc_unbound, &op);
  if (err == 0) *port = op.port;
  return err;
}


// Create a new port in this domain, which can be bound by the remote
// domain to exchange messages.
inline
int bind_unbound(domid_t pal, evtchn_port_t *port) {
  return bind_unbound_from_to(DOMID_SELF, pal, port);
}


// Connect to a port so as to allow the exchange of notifications with the pal.
inline
int bind_interdomain(domid_t pal, evtchn_port_t remote_port,
          evtchn_port_t *local_port)
{
  int err;
  evtchn_bind_interdomain_t op = { .remote_dom = pal
                                 , .remote_port = remote_port
                                 };
  err = HYPERVISOR_event_channel_op(EVTCHNOP_bind_interdomain, &op);
  if (err == 0) *local_port = op.local_port;
  return err;
}


// Unbind the given port.
// NOTE: Does not do anything with the event bits in the shared info page.
inline
int unbind(evtchn_port_t port ) {
  struct evtchn_close close;
  close.port = port;
  return HYPERVISOR_event_channel_op(EVTCHNOP_close, &close);
}


