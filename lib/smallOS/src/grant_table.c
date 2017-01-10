//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/grant_table.h>
#include <smallOS/mem.h>

unsigned long setup_remote_grant_v1(uint32_t domid)
{
	int err;
	unsigned long frame;
	gnttab_setup_table_t op;
	op.dom = domid;
	op.nr_frames = 1;
	set_xen_guest_handle(op.frame_list, &frame);
	op.status = 0;

	err = HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &op, 1);
	if (err != 0) return -1;
	if (op.status != GNTST_okay) return -1;

	return frame;
}

// Setup some pages for grant_tables.
// The "va" pointer should be page aligned---it is where the grant table
// should be placed in virtual space.
// The addresses for the grant tables should already be backup up by
// page tables (no entries in L1 requires though)

int setup_grant_table(grant_entry_v2_t *va, unsigned long page_num) {
  gnttab_setup_table_t op;
  gnttab_set_version_t version;
  mfn_t        frame[page_num];
  mmu_update_t upd  [page_num];
  int err;

  if ( ((unsigned long)va & PAGE_MASK) != 0 ) return -89;

  version.version = 2;
  err = HYPERVISOR_grant_table_op(GNTTABOP_set_version, &version, 1);
  if (err != 0) return err;

  op.dom       = DOMID_SELF;
  op.nr_frames = page_num;
  set_xen_guest_handle(op.frame_list, (unsigned long*)frame);

  err = HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &op, 1);
  if (err != 0) return err;

  if (op.status != GNTST_okay) return op.status;

  { int i;
    void *p;
    for (i = 0, p = va; i < page_num; ++i, p += PAGE_SIZE) {
      upd[i].ptr = maddr_num(virt_to_maddr(p,L1));
      upd[i].val = L1_PROT | (frame[i] << 12);
    }
  }
  err = HYPERVISOR_mmu_update(upd, page_num, NULL, DOMID_SELF); 

  return err;
}


// grant_table_find_slots searches for the first @slot_num unallocated
// grant entries in @tab and returns the number that were found.
size_t grant_table_find_slots( grant_entry_v2_t tab[], size_t entry_num
                             , grant_ref_t refs[], size_t slot_num
                             ) {
  size_t i, done;

  for (i = 0, done = 0; i < entry_num && done < slot_num; ++i) {
    if (tab[i].hdr.flags == GTF_invalid) {
      refs[done++] = i;
    }
  }

  return done;
}

int unmap_grant_handles(grant_handle_t handles[], void *addrs[], size_t n) {
  int i;
  gnttab_unmap_grant_ref_t ops[n];

  for (i = 0; i < n; ++i) {
    ops[i].handle       = handles[i];
    ops[i].host_addr    = (unsigned long)addrs[i];
    ops[i].dev_bus_addr = 0;
  } 

  return HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, ops, n);
}

int unmap_linear_grant_handles(grant_handle_t handles[], void *addr0, size_t n) {
  unsigned long i;
  gnttab_unmap_grant_ref_t ops[n];

  for (i = 0; i < n; ++i) {
    ops[i].handle       = handles[i];
    ops[i].host_addr    = (unsigned long)addr0 + PAGE_SIZE * i;
    ops[i].dev_bus_addr = 0;
  } 

  return HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, ops, n);
}
