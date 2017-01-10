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
#include "dom_mem.generic.c"
#undef ARCH

#define ARCH x86_32pae
#include "dom_mem.generic.c"
#undef ARCH

