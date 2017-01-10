// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#ifdef ARCH

// NOTE: the values for page tables in the "pt_level_t" are 1 .. 4.
// Instead of having to remember to always subtract 1,
// we use an array with 5 elements, and we ignore entry 0.


// Compute the total number of page tables.
static inline
unsigned long page_table_num(unsigned int table_num[5]) {
  unsigned long total = 0;
  pt_level_t i;
  for (i = L1; i <= root_table_lvl; ++i)
    total += (unsigned long)table_num[i];
  return total;
}

// Update the number of page tables that are needed at a given level,
// to accomodate the entreis for the page table levels at the previous level.
// Returns 0 on success, and -1 on error.
static inline
int update_table_num(unsigned int table_num[5], pt_level_t lvl) {
  unsigned int need = number_of_units(table_num[lvl - 1], PT_ENTRIES);

  if (need <= table_num[lvl]) return 0;
  table_num[lvl] = need;
  return (lvl < root_table_lvl) 
       ? update_table_num(table_num,lvl + 1)
       : -1;
}


// An iterative computation to determine how many page tables we
// need to accomodate a certain amount of virtual memory
// (including mappings for the page tables themselves).
// "other" specified how many pages we need, without space for page tables.
// Returns the total numer of page tables, or 0 if there was not
// enough space to accomodate the request (this should be very rare!)
static inline
unsigned long compute_table_num( vaddr_t(ARCH) start_page
                               , unsigned long other
                               , unsigned int table_num[5]
                               , unsigned long *mapped_pages
                               ) {
  pt_level_t lvl;
  table_num[PHYS] = 0;  // unused
  for (lvl = L1; lvl <= root_table_lvl; ++lvl) table_num[lvl] = 1;

  // On x86_32pae, we allocate enough L2 page table to go to the
  // end of the virtual space.
  if (is_pae) {
    table_num[L2] = 4 - table_offset(L3, start_page * PAGE_SIZE);
  }

  while (1) {
    unsigned long mapped_mem;
    unsigned long padding;
    unsigned long need;
    const unsigned long pages4M = 4 * 1024 * 1024 / PAGE_SIZE;
    vaddr_t(ARCH) end;
    mapped_mem  = other + page_table_num(table_num);
    end         = start_page + mapped_mem;
    // Mapped memory should end on a 4MB aligned boundary.
    // XXX: Why is 4MB???  Perhaps the intention was to be a super-page?
    padding     = pad_up(end, pages4M);
    // Ensure that there are at least 512K of padding at the end
    if (padding < 512 * 1024 / PAGE_SIZE) padding += pages4M;

    // The padding is to conform to the notes in "xen.h".
    end        += padding;
    mapped_mem  = end - start_page;
    need        = number_of_units(mapped_mem, PT_ENTRIES);

    if (need <= table_num[L1]) {
      *mapped_pages = mapped_mem;
      return page_table_num(table_num);
    }
    table_num[L1] = need;
    if (update_table_num(table_num,L2) < 0) return 0;
  }
}




// Compute the address for the first page table at the given level.
static inline
vaddr_t(ARCH) pagetable_start
  ( vaddr_t(ARCH) start
  , unsigned int table_num[5]
  , pt_level_t lvl
  ) {
  unsigned before = 0;
  unsigned prev;
  for (prev = lvl + 1; prev <= root_table_lvl; ++prev)
    before += table_num[prev];

  return start + before * PAGE_SIZE;
}

static inline
int blank_page_table(domid_t dom
                    , vaddr_t(ARCH) vbase
                    , vaddr_t(ARCH) start
                    , unsigned long num
                    ) {
  int err = map_virt(ARCH)(dom, vbase, table, start + num * PAGE_SIZE);
  if (err != 0) return err;
  memset(table,0,PAGE_SIZE);     // Start with everything unmapped.

  return 0;
}

static
int fill_tables
  ( dom_info_t*   info        // Domain info.
  , pt_level_t    lvl         // Level of page table
  , vaddr_t(ARCH) pt_start    // Start address of page tables at this level
  , unsigned long pt_num      // How many page tables do we have
  , xen_pfn_t     pfn         // Initial physical address
  , unsigned long todo        // How many physical addresses
  )
{
  if (invalid_virt_range(ARCH)(info, pt_start, pt_num * PAGE_SIZE)) {
    DB_ERROR("Invalid page table range given to fill_tables\n");
    return -1;
  }

  domid_t dom = info->dom;
  vaddr_t(ARCH) vbase = info->elf_params.virt_base;
  
  // Number of page tables allocated.
  unsigned long pt_ix = 0;
  // Offset of initial page table.
  unsigned long ix = table_offset(lvl, vbase);

  if ((pt_num == 0) || (ix + todo > pt_num * PT_ENTRIES)) {
    DB_ERROR("Insufficient number of page tables for entries\n");
    return -1;
  }

  unsigned long prot = (_PAGE_PRESENT|_PAGE_RW|_PAGE_ACCESSED|_PAGE_USER);
  if (lvl != L1) prot |= _PAGE_DIRTY;

  int err = blank_page_table(dom, vbase, pt_start, pt_ix++);
  if (err != 0) return err;

  for (; todo > 0; --todo) {
    // Allocate new blank table if needed.
    if (ix >= PT_ENTRIES) {
      err = blank_page_table(dom, vbase, pt_start, pt_ix++);
      if (err != 0) return err;
      ix = 0;
    }

    // Get mfn for pfn.
    mfn_t(ARCH) mfn = get_pfn_mfn(ARCH)(dom, pfn);
    if (is_error_mfn(mfn)) { return mfn_errno(mfn); }

    table[ix].hi_perms = prot >> PADDR_BITS;
    table[ix].mfn = mfn;
    table[ix].lo_perms = prot;
    ++ix;
    ++pfn;
  }

  // Clear up any remaining tables at this level.
  for (; pt_ix < pt_num; ++pt_ix) {
    err = blank_page_table(dom, vbase, pt_start, pt_ix);
    if (err != 0) return err;
  }

  return 0;
}

static inline
int read_only_pts
  ( dom_info_t *info              // General guest info
  , vaddr_t(ARCH) start           // The start of all page tables
  , vaddr_t(ARCH) l1_pt_start     // The start of L1 page tables
  , unsigned long pt_num          // How many page tables we have
  ) {

  int err;


  domid_t dom = info->dom;
  vaddr_t(ARCH) vbase = info->elf_params.virt_base;

  xen_pfn_t pfn = (start - vbase) >> PAGE_BITS;
  unsigned long ix  = table_offset(L1,vbase) + pfn;

  vaddr_t(ARCH) l1_tab = l1_pt_start + (ix / PT_ENTRIES) * PAGE_SIZE;
  ix                  %= PT_ENTRIES;

  if (invalid_virt_addr(ARCH)(info, l1_tab)) {
    DB_ERROR("  Invalid l1_tab address\n");
    return -1;
  }

  err = map_virt(ARCH)(dom, vbase, table, l1_tab);
  if (err != 0) return err;

  for (; pt_num > 0; --pt_num) {
    if (ix >= PT_ENTRIES) {
      l1_tab += PAGE_SIZE;
      if (invalid_virt_addr(ARCH)(info, l1_tab)) {
        DB_ERROR("  Invalid l1_tab address\n");
        return -1;
      }
      err = map_virt(ARCH)(dom, vbase, table, l1_tab);
      if (err != 0) return err;
      ix = 0;
    }
    table[ix++].lo_perms &= ~_PAGE_RW;
  }

  return 0;
}

int write_page_tables(ARCH)
  ( dom_info_t* info
  , vaddr_t(ARCH) start // page aligned
  , unsigned long *ptab_num_res
  ) {

  if (invalid_virt_addr(ARCH)(info, start)) {
    DB_ERROR("  Invalid start address\n");
    return -1;
  }

  vaddr_t(ARCH) vbase = info->elf_params.virt_base;
  vaddr_t(ARCH) start_page  = vbase / PAGE_SIZE;
  
  unsigned long other_pages = (start / PAGE_SIZE) - start_page;
  unsigned int table_num[5];
  unsigned long mapped_pages = 0;
  unsigned int ptab_num =
        compute_table_num(start_page, other_pages, table_num, &mapped_pages);

  vaddr_t(ARCH) l1_start = pagetable_start(start, table_num, L1);

  int err;
  pt_level_t lvl;

  if (start < info->elf_params.virt_base) return -EINVAL;

  if (ptab_num == 0) {
    DB_ERROR("  Failed to allocate enough page tables!\n");
    return -ENOMEM;
  }

  if (mapped_pages > info->page_num) {
    DB_ERROR("  Insufficient VM memory for initial mapping"
             " (have %lu pages, need %lu pages)\n",
                                          info->page_num, mapped_pages);
    return -ENOMEM;
  }


  DB_DEBUG("  Page tables mapped at %p.\n", start);
  DB_DEBUG("  Mapping %lu of %lu pages of memory.\n",
                                                mapped_pages, info->page_num);
  DB_DEBUG("  Initializing %d page tables:\n", ptab_num);
  for (lvl = L1; lvl <= root_table_lvl; ++lvl)
    DB_DEBUG("    L%u: %u tables.\n", lvl, table_num[lvl]);


  // Fill page tables.
  err = fill_tables( info, L1
                         , l1_start, table_num[L1]
                         , 0, mapped_pages
                         );
  if (err < 0) return err;

  // Fill page directories.
  for (lvl = L2; lvl <= root_table_lvl; ++lvl) {
    vaddr_t(ARCH) pt_start = pagetable_start(start, table_num, lvl);
    vaddr_t(ARCH) pt_addr = pt_start + table_num[lvl] * PAGE_SIZE;
    // Get pfn for pt_addr.
    xen_pfn_t pfn = (pt_addr - vbase) >> PAGE_BITS;
    err = fill_tables ( info, lvl
                            , pt_start, table_num[lvl]
                            , pfn,      table_num[lvl-1]
                            );
    if (err < 0) return err;
  }

  err = read_only_pts(info, start, l1_start, ptab_num);

  // Now we tell Xen that what we just wrote contains page tables:
  // First unmap "table", otherwise we can't pin it!
  unmap_foreign_page(info->dom,table);
  //XXX: Check for invalid mfn.
  mfn_t(ARCH) mfn = get_virt_mfn(ARCH)(info->dom, vbase, start);


  err = mark_as_page_table(info->dom, root_table_lvl, mfn);
  if (err < 0) return err;

  *ptab_num_res = ptab_num;
  return 0;
}



#endif
