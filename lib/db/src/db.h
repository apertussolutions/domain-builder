// BANNERSTART
// - Copyright 2008, Galoic, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


#ifndef __DB_H__
#define __DB_H__

#include <smallOS/xen-version.h>
#include <smallOS/mem.h>
#include <smallOS/printk.h>
#include <xen/xen.h>
#include <elf-xen/libelf.h>
#include <dom_builder.h>
#include "generated/x86_64.h"
#include "generated/x86_32.h"

// Do not define this for less output.
#define DB_VERBOSE


#define POLY(x,y)               x##_##y
#define POLY_t(x,y)             x##_##y##_t

#define vaddr_t(ARCH)           POLY_t(vaddr,ARCH)
#define maddr_t(ARCH)           POLY_t(maddr,ARCH)
#define mfn_t(ARCH)             POLY_t(mfn,ARCH)
#define shared_info_t(ARCH)     POLY_t(shared_info,ARCH)
#define start_info_t(ARCH)      POLY_t(start_info,ARCH)

#define SUPERPAGE_2MB_SHIFT   9
#define SUPERPAGE_2MB_NR_PFNS (1UL << SUPERPAGE_2MB_SHIFT)
#define SUPERPAGE_1GB_SHIFT   18
#define SUPERPAGE_1GB_NR_PFNS (1UL << SUPERPAGE_1GB_SHIFT)


typedef uint64_t vaddr_t(x86_64);
typedef uint64_t maddr_t(x86_64);
typedef uint64_t mfn_t(x86_64);

typedef uint32_t vaddr_t(x86_32pae);
typedef uint64_t maddr_t(x86_32pae);
typedef uint32_t mfn_t(x86_32pae);

// the 64-bit version is declared in x86_64.h
typedef shared_info_x86_32_t shared_info_t(x86_32pae);

// the 64-bit version is declared in x86_64.h
typedef start_info_x86_32_t start_info_t(x86_32pae);


// Information about a domain that is useful during construction.
typedef struct {
  domid_t dom;                  // The id of the domain
  unsigned int page_num;        // Number of MFNs assigned to this domain
  db_guest_t guest_type;
  unsigned int cpu_num;         // Max number of CPUs


  struct elf_binary elf;        // The ELF binary for the domain.
  struct elf_dom_parms elf_params;  // ELF parameters for building.
} dom_info_t;

// Memory --------------------------------------------------------------------


// Request some machine frames for the given domain.
int populate_physmap
  ( domid_t domid           // Owner of the pages
  , unsigned int num_pages  // How many pages
  , xen_pfn_t *ptr          // Where to place the pages
  );

int allocate_superpages
  ( domid_t domid           // Owner of the pages
  , unsigned int num_pages  // How many pages
  , unsigned int order      // How many pages
  , xen_pfn_t *ptr          // Where to place the pages
  );

void reset_phys_map(void);

int hvm_create_vm(dom_info_t *info, db_build_spec *);

// Compute the PFN for a given address.  Note that this PFN may be larger
// then the available number of physical pages but we do not check this here.
static inline
xen_pfn_t get_virt_pfn(vaddr_t(x86_64) virt_base, vaddr_t(x86_64) vaddr) {
  return (vaddr - virt_base) >> PAGE_BITS;
}

// Load the program sections from an ELF file, and parse Xen notes of interest.
int load_elf
  ( dom_info_t* info
  , uint64_t is_hvm
  , uint64_t image_dize
  , void *image
  , uint8_t *view
  );

// Tell the hypervisor that a given MFN contains a page table of
// the specified level.
int mark_as_page_table
  ( domid_t dom
  , pt_level_t level
  , xen_pfn_t mfn
  );

// Setup the behavior of CPUID
int xc_cpuid_apply_policy
  ( domid_t domid
  , unsigned long is_hvm
  , int guest_64bit
  , int is_pae
  , int is_nestedhvm
  );



// Controlling domains.
// These provide a convinient interface to the various Xen hypercalls.
int set_cpuid(domid_t domid, const unsigned int *input,
                             const unsigned int *regs);
int set_hvm_param(domid_t dom, uint32_t param, uint64_t value);
int set_address_size(domid_t dom, uint32_t size);
int set_machine_address_size(domid_t dom, uint32_t size);
int create_domain(domid_t domid, uint32_t ssidref, uint32_t flags, domid_t *dom);
int get_shared_frame(domid_t domid, mfn_t(x86_64) *res);
int set_max_vcpus(domid_t domid, uint32_t max);
int set_max_mem(domid_t domid, uint64_t max);
int set_vcpu_context(domid_t domid, uint32_t vcpu, void *ctxt);
int hypercall_init(domid_t domid, mfn_t(x86_64) mfn);

int setup_permissions(domid_t dom, const db_perms_t *spec);




int irq_perms(domid_t domid, unsigned irq, unsigned allow);
int iomem_perms(domid_t domid, mfn_t(x86_64) mfn, unsigned long num, unsigned allow);
int ioport_perms(domid_t domid, unsigned port, unsigned num, unsigned allow);

int set_shadow_allocation(domid_t domid, uint32_t mbs);

#define get_pfn_mfn(ARCH)        POLY(get_pfn_mfn,ARCH)
#define invalid_virt_addr(ARCH)  POLY(invalid_virt_addr,ARCH)
#define invalid_virt_range(ARCH) POLY(invalid_virt_range, ARCH)
#define get_virt_mfn(ARCH)       POLY(get_virt_mfn,ARCH)
#define copy_mem(ARCH)           POLY(copy_mem,ARCH)
#define fill_mem(ARCH)           POLY(fill_mem,ARCH)
#define map_virt(ARCH)           POLY(map_virt,ARCH)
#define write_page_tables(ARCH)  POLY(write_page_tables,ARCH)
#define alloc_guest_mfns(ARCH)   POLY(alloc_guest_mfns,ARCH)
#define setup_shared_info(ARCH)  POLY(setup_shared_info,ARCH)
#define setup_registers(ARCH)    POLY(setup_registers,ARCH)
#define load_elf_sections(ARCH)  POLY(load_elf_sections,ARCH)

#define ARCH x86_64
#include "db.generic.h"
#undef ARCH

#define ARCH x86_32pae
#include "db.generic.h"
#undef ARCH



// Misc. utilities

// How much do we need to add to "x", to make it a multiple of "y".
// Example: pad_up (13,10) = 7
#define pad_up(x, y)          (((y)-1) - ((x) + (y) - 1) % (y))

// How many units of "y" do we need to accomodate an "x" amount.
// PROP: y * number_of_units(x,y) = x + k,  0 <= k < y
// Example: number_of_units(7,4) = 2
#define number_of_units(x,y)  (((x) + (y) - 1) / (y))


#define DB_ERROR(args...) printk("[DB] " args)

#ifdef DB_VERBOSE
  #define DB_DEBUG(args...) printk(args)
#else
  #define DB_DEBUG(args...)
#endif

#define DB_ASSERT(p)


#endif
