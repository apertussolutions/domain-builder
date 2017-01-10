// BANNERSTART
// Copyright: 2006-2008, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki
// BANNEREND
#ifndef __HEAP_MM_H__
#define __HEAP_MM_H__

#include <stddef.h>

struct free_block;
typedef struct free_block free_block_t;
typedef free_block_t*     heap_t;

void  init_heap   (heap_t *heap, void* space, size_t bytes);
void  heap_stats  (heap_t  heap, size_t *tot_free, size_t *max_free);
void* malloc_in   (heap_t *heap, size_t bytes);
void  free_in     (heap_t *heap, void *ptr);
void* realloc_in  (heap_t *heap, void *ptr, size_t bytes);
void* calloc_in   (heap_t *heap, size_t el_num, size_t el_bytes);


#endif


