// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#ifdef ARCH


int invalid_virt_range(ARCH)(const dom_info_t* info,
                             vaddr_t(ARCH) vaddr,
                             size_t size) {
  vaddr_t(ARCH) vbase     = info->elf_params.virt_base;
  vaddr_t(ARCH) mem_bytes = (info->page_num << PAGE_BITS);
  vaddr_t(ARCH) offset;

  if (vaddr < vbase) {
    DB_ERROR("vaddr %p before vbase (%p)\n", vaddr, vbase);
    return -EINVAL;
  }

  offset = vaddr - vbase;

  if (offset >= mem_bytes) {
    DB_ERROR("vaddr-start (%p) after end of memory (%p,%p)\n",
                              vaddr, offset, mem_bytes);
    return -EINVAL;
  }

  mem_bytes -= offset;

  if (size >= mem_bytes) {
    DB_ERROR("virt region starting at %p does not fit in memory "
             "(have %p, need %p)\n", vaddr, mem_bytes, size);
    return -EINVAL;
  }

  return 0;
}


// XXX: This seems like a bit of questionable function, because
// withough the size it is not clear what we are checking.
// Furthermore, this sort of thing should be checked when we convert to
// an MFN...
inline
int invalid_virt_addr(ARCH)(const dom_info_t* info, vaddr_t(ARCH) vaddr) {
  return invalid_virt_range(ARCH)(info, vaddr, 1);
}



// Get the MFN that should be used to backup a given virtual address.
// Returns -1, if we do not have enough MFNs to accomodate a continuous mapping.
inline
mfn_t(ARCH) get_virt_mfn(ARCH)(domid_t dom,
                               vaddr_t(ARCH) vbase,
                               vaddr_t(ARCH) vaddr) {
  // Get pfn for address.
  xen_pfn_t pfn = (vaddr - vbase) >> PAGE_BITS;
  return get_pfn_mfn(ARCH)(dom, pfn);
}

// Backup the "view" with the MFN for a given virtual address in
// the domain under construction.  Note that if the address is not aligned
// on a page boundary, then it may end up in the middle of 'view'.
// Returns 0 on success or a negative error.
inline
int map_virt(ARCH)(domid_t dom, vaddr_t(ARCH) vbase, void* view, vaddr_t(ARCH) vaddr) {
  // Get pfn for address.
  mfn_t(ARCH) mfn = get_virt_mfn(ARCH)(dom, vbase, vaddr);
  if (is_error_mfn(mfn)) {
    DB_DEBUG("Failed to get MFN for %p (%d)\n", vaddr, mfn_errno(mfn));
    return -EINVAL;
  }

  return map_foreign_page(dom, view, mfn, _PAGE_PRESENT | _PAGE_RW);
}

// Copy data from our address space into the guest's address space.
int copy_mem(ARCH)
  ( const dom_info_t* info  // Information about the domain
  , vaddr_t(ARCH) dest      // Destination (in constructed domain's virt. space)
  , const uint8_t* src      // Source (in our address space)
  , size_t size             // Size of source in bytes
  , uint8_t *view           // A page of virtual space to use for copying
  ) {

#ifdef CHECKED_BUILD
  if (invalid_virt_range(ARCH)(info, dest, size)) { return -1; }
#endif

  domid_t dom = info->dom;
  vaddr_t(ARCH) vbase = info->elf_params.virt_base;

  // We copy the data one page at a time.
  while (size > 0) {
    int err;
    unsigned long start = dest % PAGE_SIZE;
    unsigned long todo  = PAGE_SIZE - start;

    if (todo > size) todo = size;

    err = map_virt(ARCH)(dom, vbase, view, dest);
    if (err != 0) return err;         // Out of memory, or something is wrong.

    memcpy(view + start, src, todo);

    dest += todo;
    src  += todo;
    size -= todo;
  }
  return 0;
}

// Fill a region of memory in the constructed domain with a particular value.
int fill_mem(ARCH)
  ( const dom_info_t* info  // Information about the domain
  , vaddr_t(ARCH) dest      // Destination (in constructed domain's virt. space)
  , uint8_t val             // Value to use for filling.
  , size_t size             // How many bytes to fill.
  , uint8_t *view
  ) {

#ifdef CHECKED_BUILD
  if (invalid_virt_range(ARCH)(info, dest, size)) { return -1; }
#endif

  domid_t dom = info->dom;
  vaddr_t(ARCH) vbase = info->elf_params.virt_base;

  // Now that we are aligned, we copy the reast one page at a time.
  while (size > 0) {
    int err;
    unsigned long start = dest % PAGE_SIZE;
    unsigned long todo  = PAGE_SIZE - start;

    if (todo > size) todo = size;

    err = map_virt(ARCH)(dom, vbase, view, dest);
    if (err != 0) return err;         // Out of memory, or something is wrong.

    memset(view + start, val, todo);

    dest += todo;
    size -= todo;
  }

  return 0;
}

#endif
