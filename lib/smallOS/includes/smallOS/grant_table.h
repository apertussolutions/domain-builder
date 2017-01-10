//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_GRANT_TABLE_H__
#define __SMALLOS_GRANT_TABLE_H__

#include <smallOS/xen-version.h>
#include <smallOS/mem.h>
#include <xen/grant_table.h>

unsigned long setup_remote_grant_v1(uint32_t domid);
int setup_grant_table(grant_entry_v2_t *va, unsigned long page_num);
int unmap_grant_handles(grant_handle_t handles[], void *addrs[], size_t n);
int unmap_linear_grant_handles(grant_handle_t handles[], void *addr0, size_t n);


static inline
int map_grant_ref(domid_t from, grant_ref_t ref, void *va, grant_handle_t *h) {
  int err;
  unsigned long addr = (unsigned long)va;
  gnttab_map_grant_ref_t op = { .host_addr  = addr
                              , .flags      = GNTMAP_host_map
                              , .ref        = ref
                              , .dom        = from
                              };
  if ((addr & PAGE_MASK) != 0) return -90;
  err = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &op, 1);
  if (err != 0) return err;
  if (op.handle < 0) return op.handle;
  if (op.status != GNTST_okay) return op.status;

  *h = op.handle;
  return 0;
}

size_t grant_table_find_slots( grant_entry_v2_t tab[], size_t entry_num
                             , grant_ref_t refs[],  size_t ref_num
                             );

static inline
void grant_table_set ( grant_entry_v2_t tab[], grant_ref_t ref
                     , domid_t dom, void *shared, uint16_t flags
                     ) {
  tab[ref].full_page.hdr.domid = dom;
  tab[ref].full_page.frame = lookup_container_mfn(shared,PHYS);
  wmb();
  tab[ref].full_page.hdr.flags = flags;
}


static inline
int grant_table_free( grant_entry_v2_t tab[], grant_ref_t ref ) {
  grant_entry_v2_t *ent = &tab[ref];
  uint16_t flags = ent->hdr.flags;
  if (flags & (GTF_reading|GTF_writing)) return -1;
  if (!__sync_bool_compare_and_swap(&ent->hdr.flags, flags, 0)) return -2;
  return 0;
}



#endif

