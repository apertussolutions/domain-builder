// BANNERSTART
// Copyright: 2010-2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki
//
// Copyright: 2003 - Rolf Neugebauer - Intel Research Cambridge
// License-file: miniOS-FreeBSD-LICENSE
// Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
// Changes: Grzegorz Milos (gm281@cam.ac.uk) 
// BANNEREND
#ifndef __STDLIB_H__
#define __STDLIB_H__

#include <heap/mm.h>

unsigned long simple_strtoul(const char *cp,char **endp, unsigned int base);
long simple_strtol(const char *cp,char **endp, unsigned int base);
unsigned long long simple_strtoull
                        (const char *cp,char **endp,unsigned int base);
long long simple_strtoll(const char *cp,char **endp,unsigned int base);
int atoi(const char *);



// -----------------------------------------------------------------------------
// Malloc and friends

extern heap_t global_heap;

#define INIT_GLOBAL_HEAP(space) init_heap(&global_heap, space, sizeof(space))

static inline
void* malloc  (size_t bytes) {
  return malloc_in(&global_heap, bytes);
}

static inline
void  free    (void *ptr) {
  free_in(&global_heap, ptr);
}

static inline
void* realloc (void *ptr, size_t bytes) {
  return realloc_in(&global_heap, ptr, bytes);
}

static inline
void* calloc  (size_t el_num, size_t el_bytes) {
  return calloc_in(&global_heap, el_num, el_bytes);
}
// -----------------------------------------------------------------------------

#endif

