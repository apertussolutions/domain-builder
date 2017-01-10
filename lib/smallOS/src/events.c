//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

// Working with events the event bits in the shared info page.
// Based on MiniOS.

#include <smallOS/hypercall.h>
#include <smallOS/shared_info.h>
#include <smallOS/utils.h>
#include <smallOS/regs.h>


// Kernel's handler for a specific event.
extern void handle_event(evtchn_port_t port);

// Assembler interface functios in start.S.
extern void hypervisor_callback(void);


// Initially all events are without a handler and disabled
int init_events(start_info_t *si) {
  int err;

  // map the shared info page
  err = init_shared_info(si);
  if (err != 0) return err;

  err = HYPERVISOR_set_callbacks( (void*)hypervisor_callback, 0, 0);
  if (err < 0) return err;

  // Go!
  HYPERVISOR_shared_info.vcpu_info[0].evtchn_upcall_mask = 0;

  return 0;
}


// The assembly callback calls this function which, in turn,
// calls "handle_event".
void do_hypervisor_callback(pt_regs *regs) {
  unsigned long l1, l1i;
  HYPERVISOR_shared_info.vcpu_info[0].evtchn_upcall_pending = 0;  // ack!

  // l1: a map specifying which parts of the pending
  // bitmap may contain events.
  l1 = xchg(&(HYPERVISOR_shared_info.vcpu_info[0].evtchn_pending_sel), 0);

  while ( l1 != 0 ) {
    unsigned long l2, l2i;
    l1i = __ffs(l1);        // find an area containing pending buts.
    l1 &= ~(1UL << l1i);    // clear the bit for this area.

    // These are the events that we are going to handle this time around.
    // We clear the pending on the CPU, and remove the ones that are masked.
    l2 = xchg(&HYPERVISOR_shared_info.evtchn_pending[l1i], 0)
       & ~(HYPERVISOR_shared_info.evtchn_mask[l1i]);

    while (l2 != 0) {
      evtchn_port_t port;
      l2i   = __ffs(l2);      // find a marked port.
      port  = l1i * (sizeof(unsigned long) * 8) + l2i;
      handle_event(port);
      l2 &= ~(1UL << l2i);    // clear bit, and go on.
    }
  }
}





