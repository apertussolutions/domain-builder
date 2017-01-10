//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_XEN_VERSION_H__
#define __SMALLOS_XEN_VERSION_H__

// We do this here because Xen headers assume that standard
// integral types are predefined.
#include <stdint.h>

#define __XEN_TOOLS__

// Xen has a bit of a weird interface story (see xen-compat.h) for reference.
// Compiling with __TOOLS__ enables some extra functionality which we need
// (e.g., some domctl function).


#endif
