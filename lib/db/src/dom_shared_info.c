// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#include "db.h"

#define ARCH x86_64
#include "dom_shared_info.generic.c"
#undef ARCH

#define ARCH x86_32pae
#include "dom_shared_info.generic.c"
#undef ARCH

