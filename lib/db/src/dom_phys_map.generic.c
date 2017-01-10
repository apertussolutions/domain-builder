// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#ifdef ARCH


#define EPP                   (PAGE_SIZE / sizeof(mfn_t(ARCH)))
#define MAX_PMP_ENTRIES       (DB_EPP * PMP_MFNS_PAGE_NUM)

static mfn_t(ARCH) *pmp_window(ARCH) = (mfn_t(ARCH)*) pmp_window_area;

// Move the PMP window so that it contains the mapping for the given PFN.
static int map_pmp_section(ARCH)(domid_t dom, xen_pfn_t pfn) {
  xen_pfn_t pmp_page_num = pfn / EPP;

  if (mapped_page != ~0 && pmp_page_num == mapped_page) return 0;
  if (pmp_page_num >= MAX_PMP_ENTRIES) return -EINVAL;

  int err = map_foreign_page(dom, pmp_window(ARCH), pmp_mfns[pmp_page_num],
                             _PAGE_PRESENT | _PAGE_RW);
  if (err < 0) {
    DB_ERROR("failed to map foreign page %u:\n", pmp_mfns[pmp_page_num]);
    return err;
  }

  mapped_page = pmp_page_num;
  return 0;
}

// Allocate memory for PFNs starting at "start" up to,
// but not including, "next".
static
int alloc_mem(ARCH)(domid_t dom, xen_pfn_t start, xen_pfn_t next) {
  xen_pfn_t pfn;
  xen_pfn_t alloc_buf[EPP];

  for (pfn = start; pfn < next; ) {
    int err;
    unsigned long count, i;
    unsigned long start_ix = pfn % EPP;

    err = map_pmp_section(ARCH)(dom, pfn);
    if (err < 0) return err;

    // Allocate some memory from Xen
    for ( count = 0
        ; start_ix + count < EPP && pfn < next
        ; ++count, ++pfn
        ) alloc_buf[count] = pfn;

    err = populate_physmap(dom, count, alloc_buf);
    if (err < 0) return err;

    for (i = 0; i < count; ++i) {
      pmp_window(ARCH)[start_ix + i] = (mfn_t(ARCH)) alloc_buf[i];
      // XXX: In theory, this cast here could fail, if the returned MFN
      // does not fit in 32-bits.  This would require a machine wiht
      // more then 44-bits of physical memory (16T)
    }

  }
  return 0;
}


static
int copy_pmp(ARCH)(domid_t dom, xen_pfn_t start, unsigned long page_num) {
  xen_pfn_t pfn;

  for (pfn = start; page_num > 0; ) {
    unsigned long count;
    unsigned long start_ix;
    int err;

    err = map_pmp_section(ARCH)(dom, pfn);
    if (err < 0) return err;

    for ( count = 0, start_ix = pfn % EPP
        ; start_ix + count < EPP && page_num > 0
        ; ++count, ++pfn, --page_num
        ) pmp_window(ARCH)[start_ix + count] = pmp_mfns[pfn - start];
  }
  return 0;
}


// Allocate the physical memory for a domain under construction.I
int alloc_guest_mfns(ARCH)
  ( domid_t dom             // Guest domain id
  , unsigned long page_num  // How many pages
  , xen_pfn_t pmp_start     // The PFN of the first page in the PMP
  )
{
  unsigned long phys_map_num_pages;
  unsigned long i;
  int err = 0;

  mapped_page = ~0;

  // Check arguments
  if (pmp_start >= page_num) return -ENOMEM;

  // Allocate pages for the PMP
  phys_map_num_pages = number_of_units(page_num, EPP);
  if (phys_map_num_pages > MAX_PMP_ENTRIES) return -ENOMEM;

  for (i = 0; i < phys_map_num_pages; ++i) pmp_mfns[i] = pmp_start + i;
  err = populate_physmap(dom, phys_map_num_pages, pmp_mfns);
  if (err < 0) return err;

  // Now, populate the PMP, allocating memory as we go along.

  err = alloc_mem(ARCH)(dom, 0, pmp_start);
  if (err != 0) goto done;

  err = copy_pmp(ARCH)(dom, pmp_start, phys_map_num_pages);
  if (err != 0) goto done;

  err = alloc_mem(ARCH)(dom, pmp_start + phys_map_num_pages, page_num);
  if (err != 0) goto done;

done:
  return err;
}

/** Compute the MFN for a given PFN. */
mfn_t(ARCH) get_pfn_mfn(ARCH)(domid_t dom, xen_pfn_t pfn) {
  int err = map_pmp_section(ARCH)(dom, pfn);
  if (err < 0) {
    DB_ERROR("Failed to map PMP section for domain %d, PFN %p\n", dom, pfn);
    return error_mfn(-err);
  }
  return pmp_window(ARCH)[pfn % EPP];
}

#undef EPP
#undef MAX_PMP_ENTRIES


#endif
