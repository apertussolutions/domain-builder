//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_MEM_H__
#define __SMALLOS_MEM_H__

#include <smallOS/xen-version.h>
#include <stddef.h>
#include <xen/xen.h>
#include <xen/arch-x86_64.h>
#include <smallOS/hypercall.h>
#include <string.h>


#define PAGE_BITS               12
#define PAGE_SIZE               (1ULL << PAGE_BITS)
#define PAGE_MASK               (PAGE_SIZE - 1)


#define L1_SHIFT                PAGE_BITS
#define L2_SHIFT                (L1_SHIFT + PT_IX_BITS)
#define L3_SHIFT                (L2_SHIFT + PT_IX_BITS)
#define L4_SHIFT                (L3_SHIFT + PT_IX_BITS)

#define PT_IX(lvl,va)           ((((unsigned long)(va)) >> lvl) & PT_IX_MASK)


// These are page-table limitations. Current CPUs support only 40-bit phys.
#define VADDR_BITS              (L4_SHIFT + PT_IX_BITS)
#define PADDR_BITS              52
#define PADDR_MASK              ((1ULL << PADDR_BITS)-1)
#define VADDR_MASK              ((1ULL << VADDR_BITS)-1)


#define KB(x) (1024 * x)
#define MB(x) (1024 * KB(x))
#define GB(x) (1024 * MB(x))


#define PAGE_ALIGN(addr)        (((addr)+PAGE_SIZE-1)&(~PAGE_MASK))

#define PAGE_ALIGNED            __attribute__ ((aligned(PAGE_SIZE)))

// Various types of global variables.

// Just for completenes, this goes in .bss
#define RESET_ZERO

#define RESET_NON_ZERO \
  __attribute__ ((section(".data#")))

#define NO_RESET \
  __attribute__ ((section("noreset,\"aw\"#")))

#define RESET_VIRT \
   __attribute__ ((section("virtual_reset,\"aw\",\"nobits\"#")))

#define NO_RESET_VIRT \
  __attribute__ ((section("virtual_noreset,\"aw\",\"nobits\"#")))

#define PAGE_TABLE \
  PAGE_ALIGNED \
  __attribute__ ((section("page_tables,\"aw\",\"nobits\"#")))





// NOTE: on PAE L3 has only 2 bits

#define PT_IX_BITS              9ULL
#define PT_ENTRIES              (1ULL << PT_IX_BITS)
#define PT_IX_MASK              (PT_ENTRIES - 1ULL)

typedef enum { PHYS = 0, L1, L2, L3, L4 } pt_level_t;

/*
typedef union {
  struct {
    uint64_t offset : PAGE_BITS;
    uint64_t page   : VADDR_BITS - PAGE_BITS;
    uint64_t pad    : 16;
  };

  uint64_t num;
  void *ptr;

} vaddr_t;

static inline
vaddr_t ptr_to_vaddr(void *p) { vaddr_t res; res.ptr = p; return res; }
*/

typedef uint64_t mfn_t;

static inline
mfn_t error_mfn(uint64_t err) {
  return (1ULL << 63) | err;
}

static inline
int is_error_mfn(mfn_t mfn) {
  return ((1ULL << 63) & mfn) > 0ULL;
}

static inline
int mfn_errno(mfn_t mfn) {
  return ~(1ULL << 63) & mfn;
}


typedef struct { uint64_t num; } pfn_t;

typedef union {
  struct {
    uint64_t lo_perms : 12;
    uint64_t mfn      : 40;
    uint64_t hi_perms : 12;
  };
  uint64_t num;
} pt_entry_t;

typedef struct {
    uint64_t num;
} maddr_t;

static inline uint64_t maddr_offset(maddr_t maddr) {
  return maddr.num & ( (1ULL << PAGE_BITS) - 1 );
}

static inline uint64_t maddr_mfn(maddr_t maddr) {
  return (maddr.num >> PAGE_BITS) & ( (1ULL << (PADDR_BITS - PAGE_BITS)) - 1 );
}

static inline uint64_t maddr_num(maddr_t maddr) {
  return maddr.num;
}

static inline
maddr_t make_maddr(uint64_t mfn, uint64_t offset) {
  maddr_t maddr;
  maddr.num = (mfn << PAGE_BITS) | offset;
  return maddr;
}

static inline
maddr_t mfn_to_maddr(mfn_t mfn) {
  return make_maddr(mfn, 0);
}



// The format of "perms" above:
#define _PAGE_PRESENT           (1ULL << 0)
#define _PAGE_RW                (1ULL << 1)
#define _PAGE_USER              (1ULL << 2)
#define _PAGE_PWT               (1ULL << 3)
#define _PAGE_PCD               (1ULL << 4)
#define _PAGE_ACCESSED          (1ULL << 5)
#define _PAGE_DIRTY             (1ULL << 6)
#define _PAGE_PAT               (1ULL << 7)
#define _PAGE_SUPER             (1ULL << 7)
#define _PAGE_GLOBAL            (1ULL << 8)


#define L1_PROT (_PAGE_PRESENT|_PAGE_RW|_PAGE_ACCESSED|_PAGE_USER)
#define L1_PROT_RO (_PAGE_PRESENT|_PAGE_ACCESSED|_PAGE_USER)
#define L2_PROT (_PAGE_PRESENT|_PAGE_RW|_PAGE_ACCESSED|_PAGE_DIRTY|_PAGE_USER)
#define L3_PROT (_PAGE_PRESENT|_PAGE_RW|_PAGE_ACCESSED|_PAGE_DIRTY|_PAGE_USER)
#define L4_PROT (_PAGE_PRESENT|_PAGE_RW|_PAGE_ACCESSED|_PAGE_DIRTY|_PAGE_USER)



xen_pfn_t mfn_to_pfn(mfn_t mfn);


// Extract the MFN from a page table entry.
static inline mfn_t pte_to_mfn(pt_entry_t pte) {
  return pte.mfn;
}


int init_L4_loop(void);

mfn_t lookup_container_mfn(void *va, int level);

uint64_t lookup_container_offset(void *vaddr, pt_level_t lvl);


// This function computes machines adders associated
// with a virtual address, as determined by the "level" paramter.
// If the parameter is PHYS, then we return the machine
// address associated with the virtual address.
// If the parameter is L*, then we return the physical address
// of the page table entry at the corresponding level of the mapping.
maddr_t virt_to_maddr(void *va, pt_level_t level);



// Compiler write barrier
#define mb()  asm volatile ("mfence" ::: "memory")
#define rmb() asm volatile ("lfence" ::: "memory")
#define wmb() asm volatile ("sfence" ::: "memory")


int flush_tlb(void *va);

int flush_all_tlb(void);

int unmap_foreign_page(domid_t dom, void *va);

int map_foreign_page( domid_t dom , void *va , mfn_t mfn , uint64_t perms);

void decompose_pointer(void * p, size_t * idx);
#endif
