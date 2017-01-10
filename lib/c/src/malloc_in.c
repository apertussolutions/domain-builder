// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND
#include <string.h>
#include <heap/mm.h>

struct free_block {
  size_t size;                // in units of sizeof(free_block_t)
  struct free_block *next;
};

// A "heap" is a list of free blocks.
// Lists of free blocks are assumed to be sorted by address.

static inline
size_t size_in_blocks(size_t bytes) {
  size_t sz = sizeof(free_block_t);
  return (bytes + sz + sizeof(size_t) - 1) / sz;
}

// Argument size is in bytes.
void *malloc_in(heap_t *heap, size_t size) {
  free_block_t *cur, *prev;
  size = size_in_blocks(size);

  for ( prev = NULL, cur = *heap
      ; cur != NULL
      ; prev = cur, cur = cur->next
      ) {

    if (cur->size < size) continue;

    if (cur->size > size) {
      free_block_t *new = cur + size;
      new->size = cur->size - size;
      new->next = cur->next;
      if (prev == NULL) *heap = new; else prev->next = new;
      cur->size = size;
      return &cur->next;
    }

    if (cur->size == size) {
      if (prev == NULL) *heap = cur->next; else prev->next = cur->next;
      return &cur->next;
    }
  }

  return NULL;
}


// Assumes block /= NULL
static inline
void join(free_block_t *block) {
  free_block_t *next = block->next;

  if (block + block->size == next) {
    block->size += next->size;
    block->next  = next->next;
  }
}


void free_in(heap_t *heap, void* ptr) {
  free_block_t *prev, *cur;
  free_block_t *block = (free_block_t*)(ptr - sizeof(size_t));

  if (ptr == NULL) return;

  for ( prev = NULL, cur = *heap
      ; cur != NULL && cur < block
      ; prev = cur, cur = cur->next
      );

  block->next = cur;
  join(block);

  if (prev == NULL) {
    *heap = block;
    return;
  }

  prev->next = block;
  join(prev);
}

// Size is in bytes
void *realloc_in(heap_t *heap, void *ptr, size_t size) {
  free_block_t *block = (free_block_t*)(ptr - sizeof(size_t));
  size_t our_size, need;
  free_block_t *prev, *cur;
  void *moved;

  if (ptr == NULL) return malloc_in(heap, size);
  if (size == 0) { free_in(heap, ptr); return NULL; }

  size = size_in_blocks(size);
  our_size = block->size;

  // No change
  if (size == our_size) return ptr;

  // Decrease size
  if (size < our_size) {
    free_block_t *new = block + size;
    new->size = our_size - size;
    block->size = size;
    free_in(heap, &new->next);
    return ptr;
  }

  // Try to enlarge the block
  for ( prev = NULL, cur = *heap
      ; (cur != NULL) && (cur < block)
      ; prev = cur, cur = cur->next
      );

  need = size - our_size;

  // Is it possible to extend it without moving?
  if (cur == block + our_size && need <= cur->size) {
    free_block_t *new;

    block->size += need;
    if (cur->size == need) {
      new = cur->next;
    } else {
      new       = cur + need;
      new->size = cur->size - need;
      new->next = cur->next;
    }

    if (prev == NULL) *heap = new; else prev->next = new;
    return ptr;
  }

  // We'll have to move things around.
  moved = malloc_in(heap, size);
  if (moved == NULL) return NULL;

  memcpy(moved, ptr, our_size * sizeof(free_block_t) - sizeof(size_t));
  free_in(heap, ptr);

  return moved;
}


void *calloc_in(heap_t *heap, size_t m, size_t n) {
  size_t bytes = m * n;
  void *p = malloc_in(heap, bytes);
  if (p == NULL) return p;
  memset(p, 0, bytes);
  return p;
}

void init_heap(heap_t *heap, void *space, size_t bytes) {
  size_t blocks = bytes / sizeof(free_block_t);
  if (blocks == 0) { *heap = NULL; return; }

  *heap = space;
  (*heap)->size = blocks;
  (*heap)->next = NULL;
}


void heap_stats(heap_t heap, size_t *tot_free, size_t *max_free) {
  size_t tot = 0, max = 0;
  free_block_t *cur;

  for (cur = heap; cur != 0; cur = cur->next) {
    tot += cur->size;
    if (cur->size > max) max = cur->size;
  }

  *tot_free = tot;
  *max_free = max;
}





