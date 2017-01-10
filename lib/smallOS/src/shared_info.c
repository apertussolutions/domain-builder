//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

// Working with events the event bits in the shared info page.

#include <smallOS/hypercall.h>
#include <smallOS/shared_info.h>


// This is the shared-info page for communicating with the hypervisor.
volatile
shared_info_t HYPERVISOR_shared_info NO_RESET_VIRT PAGE_ALIGNED;

int init_shared_info(start_info_t *si) {
  int i;
  int err;

  // map the shared info page
  err = HYPERVISOR_update_va_mapping
          ( (unsigned long)(&HYPERVISOR_shared_info)
          , (si->shared_info) | 7
          , UVMF_INVLPG
          );

  if (err != 0) return err;

  for(i = 0; i < NR_EVENT_CHANNELS; ++i)
    mask_evtchn(i);

  return 0;
}



