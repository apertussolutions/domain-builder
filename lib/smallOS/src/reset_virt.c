//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/mem.h>

extern uint8_t _virtual_reset_start[][PAGE_SIZE];
extern uint8_t _virtual_end[][PAGE_SIZE];

int reset_virtual_region(void) {
  uint8_t (*start)[PAGE_SIZE] = _virtual_reset_start;
  size_t pages_todo           = _virtual_end - start;
  size_t this_time;

  for ( pages_todo = _virtual_end - start
      ; pages_todo > 0
      ; pages_todo -= this_time
      ) {

    mmu_update_t update[PT_ENTRIES];    // We unmap things 2MB at a time
    size_t i;
    int err;

    this_time = PT_ENTRIES;
    if (this_time > pages_todo) this_time = pages_todo;

    for (i = 0; i < this_time; ++i) {
      update[i].ptr = maddr_num(virt_to_maddr(start, L1));
      update[i].val = 0;
      start++;
    }

    err = HYPERVISOR_mmu_update(update, this_time, NULL, DOMID_SELF);
    if (err != 0) return err;

  }

  return 0;
}


