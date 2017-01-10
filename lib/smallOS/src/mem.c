//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/mem.h>

// From link.lds
extern pt_entry_t page_table[PT_ENTRIES][PT_ENTRIES][PT_ENTRIES][PT_ENTRIES];

static inline
size_t L4_LOOP_ENTRY(void) {
  extern uint8_t _L4_LOOP_ENTRY[];    // From link.lds
  return (size_t)_L4_LOOP_ENTRY;      // Not an address!
}

// This is the L2 page table for the virtual space region.
static pt_entry_t virtual_L2_table[PT_ENTRIES] PAGE_TABLE;

// These 3 are defined in the linker script:
extern pt_entry_t virtual_L1_tables[][PT_ENTRIES];    // From link.lds
extern uint8_t _virtual_start[];                      // From link.lds

static inline
size_t virtual_L1_table_num(void) {
  extern uint8_t _virtual_L1_table_num[]; // From link.lds
  return (size_t)_virtual_L1_table_num;   // Not an address!
}


void decompose_pointer(void * p, size_t * idx) {
  uintptr_t u = (uintptr_t)p;
  idx[0] = (u >> L4_SHIFT) & 0x1ff;
  idx[1] = (u >> L3_SHIFT) & 0x1ff;
  idx[2] = (u >> L2_SHIFT) & 0x1ff;
  idx[3] = (u >> L1_SHIFT) & 0x1ff;
  idx[4] = u               & 0xfff;
}


mfn_t lookup_container_mfn(void *va, int level) {
  size_t in_idx[5];
  size_t out_idx[4];
  size_t out_i = 0, in_i = 0;

  decompose_pointer(va, in_idx);

  switch (level) {
    case L4: out_idx[out_i++] = L4_LOOP_ENTRY(); /* @fallthrough@ */
    case L3: out_idx[out_i++] = L4_LOOP_ENTRY(); /* @fallthrough@ */
    case L2: out_idx[out_i++] = L4_LOOP_ENTRY(); /* @fallthrough@ */
    case L1: out_idx[out_i++] = L4_LOOP_ENTRY(); /* @fallthrough@ */
    case PHYS: break;
  }

  while(out_i < 4) {
    out_idx[out_i++] = in_idx[in_i++];
  }

  return pte_to_mfn(page_table[out_idx[0]][out_idx[1]][out_idx[2]][out_idx[3]]);
}

uint64_t lookup_container_offset(void *vaddr, pt_level_t lvl) {
  size_t idx[5];
  decompose_pointer(vaddr, idx);
  switch(lvl) {
    case PHYS: return idx[4];
    case L1:   return idx[3] * sizeof (pt_entry_t);
    case L2:   return idx[2] * sizeof (pt_entry_t);
    case L3:   return idx[1] * sizeof (pt_entry_t);
    case L4:   return idx[0] * sizeof (pt_entry_t);
    default:   return -1;
  }
}

maddr_t virt_to_maddr(void *va, pt_level_t level) {
  return make_maddr( lookup_container_mfn(va, level)
                   , lookup_container_offset(va, level));
}

int flush_tlb(void *va) {
  mmuext_op_t flush_req = { .cmd = MMUEXT_INVLPG_LOCAL
                          , .arg1.linear_addr = (unsigned long)va
                          , .arg2.nr_ents = 0 // initialized unused second argument
                          };

  return HYPERVISOR_mmuext_op(&flush_req, 1, NULL, (domid_t)DOMID_SELF);
}

int flush_all_tlb(void) {
  mmuext_op_t flush_req = { .cmd = MMUEXT_TLB_FLUSH_ALL
                          };

  return HYPERVISOR_mmuext_op(&flush_req, 1, NULL, (domid_t)DOMID_SELF);
}






int map_foreign_page
  ( domid_t dom           // owner of physical memroy
  , void *va             // location in our address space
  , mfn_t mfn             // physical memory
  , uint64_t perms   // permissions (both hi & low, in their correct places)
  ) {

  mmu_update_t update;

  update.ptr = maddr_num(virt_to_maddr(va,L1));
  update.val = (mfn << PAGE_BITS) | perms;

  (void)flush_tlb(va);
  return HYPERVISOR_mmu_update(&update, 1, NULL, dom);
}

int unmap_foreign_page(domid_t dom, void *va) {
  return map_foreign_page(dom, va, 0ULL, 0);
}

xen_pfn_t mfn_to_pfn(mfn_t mfn) {
  return machine_to_phys_mapping[mfn];
}



static
int init_virtual_space(void) {
  size_t i;
  mmu_update_t update;
  pt_entry_t entry;

  memset(virtual_L2_table, 0, PAGE_SIZE);
  for (i = 0; i < virtual_L1_table_num(); ++i) {
    mfn_t mfn = lookup_container_mfn(virtual_L1_tables[i], PHYS);
    virtual_L2_table[i].lo_perms = L2_PROT;
    virtual_L2_table[i].mfn = mfn;
    virtual_L2_table[i].hi_perms = 0;
    memset(virtual_L1_tables[i], 0, PAGE_SIZE);
    unmap_foreign_page(DOMID_SELF, virtual_L1_tables[i]);
  }

  entry.lo_perms = L3_PROT;
  entry.mfn      = lookup_container_mfn(virtual_L2_table, PHYS);
  entry.hi_perms = 0;

  update.ptr = maddr_num(virt_to_maddr(_virtual_start, L3));
  memcpy(&update.val, &entry, sizeof(entry));

  // XXX: Check error?
  (void) unmap_foreign_page(DOMID_SELF, virtual_L2_table);
  return HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF);
}


int init_L4_loop(void) {
  maddr_t root;
  maddr_t addr;
  mmu_update_t update;
  int err;

  asm volatile ("movq %%cr3, %0" : "=r" (root));

  addr = make_maddr(maddr_mfn(root), L4_LOOP_ENTRY() * sizeof(pt_entry_t));

  update.ptr = maddr_num(addr);
  update.val = (maddr_mfn(root) << PAGE_BITS) | _PAGE_PRESENT;

  err = HYPERVISOR_mmu_update(&update, 1, NULL, (domid_t)DOMID_SELF);
  if (err != 0) return err;

  return init_virtual_space();
}


