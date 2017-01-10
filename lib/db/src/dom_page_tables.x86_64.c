// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


/*
 * This module provides functions for manipulating the new virtual
 * machine's memory.  This is the most complex part of the domain builder.
 * We have functions that:
 *   - translate between virtual addresses, pseudo-physical frame numbers (PFN),
 *     and machine frame numbers (MFN)
 *   - functions to copy data from the address space of the domain builder into
 *     the address space of the virtual machine.
 *   - functions to initialize the page tables of the new virtual machine.
 *   - functions to manipulate the start_info page of the new virtual machine.
 */


#include "db.h"
#include <string.h>
#include <errno.h>


#define ARCH x86_64


// A "window" of virtual space.  It is used to initialize page tables.
static pt_entry_t       table[PT_ENTRIES] RESET_VIRT PAGE_ALIGNED;
static const pt_level_t root_table_lvl  = L4;
static const int        is_pae          = 0;

static inline
unsigned table_offset(pt_level_t lvl, vaddr_t(ARCH) x) {
  switch (lvl) {
    case L1: return PT_IX(L1_SHIFT,x);
    case L2: return PT_IX(L2_SHIFT,x);
    case L3: return PT_IX(L3_SHIFT,x);
    case L4: return PT_IX(L4_SHIFT,x);

    default:
     DB_ERROR("\"tables_offset (x86_64)\" unsupported level %d\n", lvl);
     return 0;
  }
}

#include "dom_page_tables.generic.c"

#undef ARCH
// -----------------------------------------------------------------------------



