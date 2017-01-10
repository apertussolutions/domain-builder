// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


#include "db.h"
#include <string.h>
#include <errno.h>



// -----------------------------------------------------------------------------

// Physical to Machine Map (PMP)
//
// For a 36-bit physical space, the largest PMP has 2^(36 - 12) = 2 ^ 24
// entries.  In one page we can fit 2^10 entries.
// (because we assume 4K pages, and 4 bytes per entry).
// So we need at most 2^14 MFNs to back-up the PMP.

// -- 64-bit -------------------------------------------------------------------
// PAGE_SIZE  = 2^12
// ENTRY_SIZE = 2^3
// 1MB        = 2^20 bytes = 2^20 / 2^12 = 2^8 (256) pages.
// EPP        = 2^12 / 2^3 = 2^9 (512) entries per page.
//
// A PMP that fits in 2^14 pages has 2^14 * 2^9 = 2^23 entries.
// This corresponds to 2^23 pages of physical memory.
// This is 2^23 / 2^8 = 2^15 MB = 32GB of physical memory,
// This is the maximum VM size that we support.
// -----------------------------------------------------------------------------

#define PMP_MFNS_PAGE_NUM 32
#define DB_EPP (PAGE_SIZE / sizeof(xen_pfn_t))

static xen_pfn_t pmp_mfns [PMP_MFNS_PAGE_NUM * DB_EPP] RESET_ZERO PAGE_ALIGNED;
static uint8_t pmp_window_area  [PAGE_SIZE] RESET_VIRT PAGE_ALIGNED;
static xen_pfn_t mapped_page NO_RESET;    // Could be made resettable

// The functions/types bellow are overloaded to work for different archs.
// The macros bellow define the overloaded interface (e.g., a Haskell class).

#define map_pmp_section(ARCH)   POLY(map_pmp_section,ARCH)
#define alloc_mem(ARCH)         POLY(alloc_mem,ARCH)
#define copy_pmp(ARCH)          POLY(copy_pmp,ARCH)
#define pmp_mfns(ARCH)          POLY(pmp_mfns,ARCH)
#define pmp_window(ARCH)        POLY(pmp_window,ARCH)

// The generic implementations are in the file "dom_phy_map.generic.c"
// Bellow, we instantiate the generic implementation to the different
// architectures (i.e., these are the "instances" of the above class)

#define ARCH x86_64
#include "dom_phys_map.generic.c"
#undef ARCH

#define ARCH x86_32pae
#include "dom_phys_map.generic.c"
#undef ARCH



