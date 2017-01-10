#ifndef ARCH
#error "db.generic.h should not be used directly"
#endif

/** Get the machine frame for a given pseudo-physical frame. */
mfn_t(ARCH) get_pfn_mfn(ARCH)(domid_t dom, xen_pfn_t pfn);

/** Returns non-zero if vaddr is not a valid virtual address in domain. */
int invalid_virt_addr(ARCH)(const dom_info_t* info, vaddr_t(ARCH) vaddr);

/** Returns non-zero if vaddr is not a valid virtual address range in domain. */
int invalid_virt_range(ARCH)(const dom_info_t* info,
                             vaddr_t(ARCH) vaddr,
                             size_t size);

// Get the MFN that should back up the given virtual address.
// The domain builder uses a contiguous 1-1 mapping.
mfn_t(ARCH) get_virt_mfn(ARCH)(domid_t dom, vaddr_t(ARCH) vbase, vaddr_t(ARCH) vaddr);


// Map the page containing the given guest address, into our space,
// at the (page aligned) address "view".
int map_virt(ARCH)(domid_t dom, vaddr_t(ARCH) vbase, void* view, vaddr_t(ARCH) vaddr);

int copy_mem(ARCH)
  ( const dom_info_t* info  // Information about the constructed domain
  , vaddr_t(ARCH) dest      // destination (in constructed domain's virt. space)
  , const uint8_t* src      // source (in our virtual space)
  , size_t size             // size of source in bytes
  , uint8_t *view           // Virtual space to use for copying
  );

// Fill a region of memory in the constructed domain with a particular value.
int fill_mem(ARCH)
  ( const dom_info_t* info        // Information about the domain
  , vaddr_t(ARCH) dest      // Destination (in constructed domain's virt. space)
  , uint8_t val             // the value that should be filled in
  , size_t size             // size of source in bytes
  , uint8_t *page           // Virtual space to use for copying
  );

// Fills in the page table for the constructed domain.
int write_page_tables(ARCH)
  ( dom_info_t* info        // Information about the constructed domain
  , vaddr_t(ARCH) start     // Address of 1st pages table (page aligned)
  , unsigned long *ptab_num
  );

int setup_shared_info(ARCH)
  ( domid_t dom
  , shared_info_t(ARCH) *shared_info
  , maddr_t *shared_info_ma
  );

// Allocate the physical memory for a domain under construction.I
int alloc_guest_mfns(ARCH)
  ( domid_t dom             // Guest domain id
  , unsigned long page_num  // How many pages
  , xen_pfn_t pmp_start     // The PFN of the first page in the PMP
  );

int setup_registers(ARCH)
  ( dom_info_t *info
  , vaddr_t(ARCH) start_info
  , maddr_t(ARCH) ptab
  );

int load_elf_sections(ARCH)
   ( dom_info_t* info
   , uint8_t *view
   );
