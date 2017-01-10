#include <smallOS/mem.h>
#include <xen/hvm/e820.h>
#include <xen/hvm/params.h>
#include <xen/hvm/hvm_info_table.h>
#include <errno.h>
#include <elf-xen/libelf.h>
#include "db.h"
#include <xen/memory.h>

// We do special pages like Xen's libxc:
#define SPECIALPAGE_BUFIOREQ 0
#define SPECIALPAGE_XENSTORE 1
#define SPECIALPAGE_IOREQ    2
#define SPECIALPAGE_IDENT_PT 3
#define NR_SPECIAL_PAGES     4
#define special_pfn(x) (0xff000u - NR_SPECIAL_PAGES + (x))


#define VGA_START_PFN 0xA0
#define VGA_LEN_PFN   0x20

#define CHUNK_PAGES (MB(2) / PAGE_SIZE)

// Window of virtual memory used to initialize memory.
static uint8_t chunk[PAGE_SIZE] RESET_VIRT PAGE_ALIGNED;

static inline size_t min(size_t a, size_t b) {
  return a < b ? a : b;
}

int set_memmap_limit(domid_t domid, size_t limit_pages) {

  struct xen_foreign_memory_map mem_map = {
    .domid = domid,
    .map   = { .nr_entries = 1 }
  };

  uint64_t e820_entry[3] = {0, (uint64_t)limit_pages * PAGE_SIZE,
                                                        1 /* E820 RAM */ };

  set_xen_guest_handle(mem_map.map.buffer, e820_entry);

  return HYPERVISOR_memory_op(XENMEM_set_memory_map, &mem_map);
}

/**
 * Allocate the chunk after the current one.  This modifies #chunk_start and
 * #chunk_size to point to the next chunk start and size respectively.
 *
 * \return 0 if successful, and non-zero error code if not.
 */
static
int alloc_mem
  ( domid_t domid
  , size_t request_pfns                   // How many PFNs we need
  , xen_pfn_t *last_pfn                   // OUT: Up to where we allocated.
  ) {

  int err;
  size_t max_shift = SUPERPAGE_1GB_SHIFT;
  xen_pfn_t chunk_mfns[CHUNK_PAGES];      // Used to allocate chunks
  xen_pfn_t new_start = 0;
  unsigned long stats[3] = { 0, 0, 0 };

  while (request_pfns > 0) {
    size_t todo, i, extents, shift, nr_pages;
    xen_pfn_t new_end;

    // If we are at the beginning of a hole, then skip over it.
    if (new_start == VGA_START_PFN)
      new_start += VGA_LEN_PFN;
    else if (new_start == HVM_BELOW_4G_MMIO_START / PAGE_SIZE)
      new_start += HVM_BELOW_4G_MMIO_LENGTH / PAGE_SIZE;

    // If the chunk spans a hole, truncate it to end before the hole.
    new_end = new_start + request_pfns;
    if (new_start < VGA_START_PFN
                 && VGA_START_PFN <= new_end) {
        new_end = VGA_START_PFN;
    } else if (new_start < HVM_BELOW_4G_MMIO_START / PAGE_SIZE
                        && HVM_BELOW_4G_MMIO_START / PAGE_SIZE <= new_end) {
        new_end = HVM_BELOW_4G_MMIO_START / PAGE_SIZE;
    }
    todo = new_end - new_start;

#define SHOULD_ATTEMPT(super_shift) \
  (max_shift >= super_shift \
   && todo >= (1UL << super_shift) \
   && (new_start % (1UL << super_shift) == 0))

#define TO_NEXT_BOUNDARY(pfns) ((pfns) - new_start % (pfns))

    // When we adjust the "todo" value we attempt to align the subsequent
    // allocation to a superpage boundary.

    if (SHOULD_ATTEMPT(SUPERPAGE_1GB_SHIFT)) {
        shift = SUPERPAGE_1GB_SHIFT;
    } else {
      todo = min(todo, TO_NEXT_BOUNDARY(SUPERPAGE_1GB_NR_PFNS));

      if (SHOULD_ATTEMPT(SUPERPAGE_2MB_SHIFT)) {
        shift = SUPERPAGE_2MB_SHIFT;
      } else {
        todo = min(todo, TO_NEXT_BOUNDARY(SUPERPAGE_2MB_NR_PFNS));

        shift = 0;
      }

    }

#undef SHOULD_ATTEMPT
#undef TO_NEXT_BOUNDARY

    nr_pages = 1UL << shift;
    extents = min(todo / nr_pages, CHUNK_PAGES);

    for (i = 0; i < extents; ++i)
      chunk_mfns[i] = new_start + i * nr_pages;

    err = allocate_superpages(domid, extents, shift, chunk_mfns);

    if (err < 0) {
      DB_ERROR("Failed to allocate memory for chunk\n");
      return err;
    }

    if (err < extents) {
      switch (shift) {
        case SUPERPAGE_1GB_SHIFT: max_shift = SUPERPAGE_2MB_SHIFT; break;
        case SUPERPAGE_2MB_SHIFT: max_shift = 0;                   break;
        default:
          DB_ERROR("Asked for %d MFNS, got %d\n", extents, err);
          return -ENOMEM;
      }
    }

    stats [ shift == SUPERPAGE_1GB_SHIFT ? 2 :
            shift == SUPERPAGE_2MB_SHIFT ? 1 :
                                           0 ] += err;

    new_start    += err * nr_pages;
    request_pfns -= err * nr_pages;
  }

  DB_DEBUG("  4K pages: %lu\n  2M pages: %lu\n  1G pages: %lu\n",
                                          stats[0], stats[1], stats[2]);

  *last_pfn = new_start;
  return 0;
}

static inline
int foreign_memcpy
  ( domid_t domid
  , unsigned long tgt /* foreign */
  , const void *src
  , size_t bytes
  ) {

  xen_pfn_t page  = tgt / PAGE_SIZE;
  size_t offset   = tgt % PAGE_SIZE;

  while (bytes > 0) {
    int err;
    size_t todo = PAGE_SIZE - offset;
    if (bytes < todo) todo = bytes;

    err = map_foreign_page(domid, chunk, page, _PAGE_PRESENT | _PAGE_RW);
    if (err != 0) return err;

    memcpy(chunk + offset, src, todo);
    bytes -= todo;
    src += todo;
    offset = 0;
    ++page;
  }

  return 0;
}

static inline
int foreign_memzero
  ( domid_t domid
  , unsigned long tgt /* foreign */
  , size_t bytes
  ) {

  xen_pfn_t page  = tgt / PAGE_SIZE;
  size_t offset   = tgt % PAGE_SIZE;

  while (bytes > 0) {
    int err;
    size_t todo = PAGE_SIZE - offset;
    if (bytes < todo) todo = bytes;

    err = map_foreign_page(domid, chunk, page, _PAGE_PRESENT | _PAGE_RW);
    if (err != 0) return err;

    memset(chunk + offset, 0, todo);
    bytes -= todo;
    offset = 0;
    ++page;
  }

  return 0;
}

#define load_phdrs(wx) ELF_POLY(load_phdrs, wx)

#define WX 64
#include "dom_hvm.generic.c"
#undef WX

#define WX 32
#include "dom_hvm.generic.c"
#undef WX

static
int gen_load_phdrs (domid_t domid, struct elf_binary *elf) {
  return elf_64bit(elf)
       ? load_phdrs(64)(domid, elf)
       : load_phdrs(32)(domid, elf);
}


static inline
uint8_t checksum(uint8_t *buf, size_t size) {
  uint8_t tot = 0;
  size_t i;
  for (i = 0; i < size; ++i)
    tot += buf[i];
  return -tot;
}


static inline
int init_start_info
  ( domid_t domid
  , xen_pfn_t mem_end
  ) {

  int err;
  xen_pfn_t low_mem_end_pfn   = HVM_BELOW_4G_RAM_END / PAGE_SIZE;
  struct hvm_info_table *info = (void*) &chunk[HVM_INFO_OFFSET];

  err = map_foreign_page(domid, chunk, HVM_INFO_PFN, _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) return err;

  memset(info, 0, sizeof(struct hvm_info_table));

  strncpy(info->signature, "HVM INFO", 8);
  info->length = sizeof(struct hvm_info_table);

  // All of these should be parameterized?
  // info->acpi_enabled = 1;
  info->apic_mode    = 1;
  info->nr_vcpus     = 1;

  if (mem_end <= low_mem_end_pfn) {
    info->low_mem_pgend = mem_end;
    info->high_mem_pgend  = 0;
  } else {
    info->low_mem_pgend = low_mem_end_pfn;
    info->high_mem_pgend  = mem_end;
  }

  info->reserved_mem_pgstart = special_pfn(0);

  info->vcpu_online[0] = 1;   // XXX: Turn on only the first CPU?

  info->checksum = checksum((uint8_t*) info, info->length);

  return 0;
}

static inline
int set_param(domid_t domid, const char *name, uint32_t p, uint64_t v) {
  int err;
  DB_DEBUG("  [HVM] Set %s to 0x%llx\n", name, v);
  err = set_hvm_param(domid, p, v);
  if (err != 0) return err;
  return 0;
}



static inline
int identity_page_table(domid_t domid) {
  uint32_t *pdir = (void*) chunk;
  unsigned long i;
  int err;

  err = map_foreign_page(domid, chunk,
             special_pfn(SPECIALPAGE_IDENT_PT), _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) {
    DB_ERROR("Failed to map page\n");
    return err;
  }

  for (i = 0; i < PAGE_SIZE / sizeof(*pdir); ++i) {
    pdir[i] = (i << 22) | _PAGE_PRESENT  | _PAGE_RW    | _PAGE_USER
                        | _PAGE_ACCESSED | _PAGE_DIRTY | _PAGE_SUPER;

  }
  return set_param(domid, "ident_pt", HVM_PARAM_IDENT_PT,
                              special_pfn(SPECIALPAGE_IDENT_PT));
}


// Insert JMP <rel32> instruction at address 0x0 to reach entry point.
static inline
int setup_entry_point
  ( domid_t domid
  , struct elf_binary *elf
  ) {

  uint64_t entry = elf->entry;


  // XXX: We should prolly check that entry is not < 5 also.
  // This is not likely, but we'll be overwriting things if so.
  if (entry != 0) {
    int err = map_foreign_page(domid, chunk, 0, _PAGE_PRESENT | _PAGE_RW);
    if (err != 0) return err;

    chunk[0] = 0xe9;
    *(uint32_t *)&chunk[1] = entry - 5;
  }

  return 0;
}

static inline
int setup_xenstore (domid_t domid, db_build_spec *spec) {
  int err;
  xen_pfn_t pfn = special_pfn(SPECIALPAGE_XENSTORE);
  evtchn_port_t port = 0;

  if (spec->xenstore.domid == ~0ULL) {
    DB_DEBUG("  [HVM] No XenStore\n");
    return 0;
  }

  err = map_foreign_page(domid, chunk, pfn, _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) { DB_ERROR("Failed to map page (%d)\n", err); return err; }

  memset(chunk, 0, PAGE_SIZE);

  err = bind_unbound_from_to(domid, spec->xenstore.domid, &port);
  if (err != 0) {
    DB_ERROR("  [HVM] Failed to create XenStore port (%d)\n", err);
    return err;
  }

  DB_DEBUG("  [HVM] XenStore: MFN %p, port %llu\n", (void*) pfn
                                                  , (unsigned long long) port);
  spec->xenstore.mfn = pfn;
  spec->xenstore.port = port;
  set_param(domid, "STORE_PFN", HVM_PARAM_STORE_PFN,    pfn);
  set_param(domid, "STORE_EVTCHN", HVM_PARAM_STORE_EVTCHN, port);

  return 0;
}

static inline
int setup_io_page(domid_t domid, char *name, uint64_t a, int p) {
  int err;
  err = map_foreign_page(domid, chunk, special_pfn(p),
                                                  _PAGE_PRESENT | _PAGE_RW);
  if (err != 0) { DB_ERROR("Failed to map page (%d)\n", err); return err; }
  memset(chunk, 0, PAGE_SIZE);
  return set_param(domid, name, a, special_pfn(p));
}


static inline
int setup_io_pages(domid_t domid) {
  int err;
  err = setup_io_page(domid, "IOREQ_PFN",  HVM_PARAM_IOREQ_PFN,
                                                      SPECIALPAGE_IOREQ);
  if (err != 0) return err;

  err = setup_io_page(domid, "BUFIOREQ_PFN",  HVM_PARAM_BUFIOREQ_PFN,
                                                      SPECIALPAGE_BUFIOREQ);
  if (err != 0) return err;

  return 0;
}


void do_test(domid_t domid, xen_pfn_t p, size_t off) {
  int err = map_foreign_page(domid, chunk, p, _PAGE_PRESENT | _PAGE_RW);
  int i;
  if (err != 0) {
    DB_ERROR("FAILED TO MAP TEST PAGE\n");
  }
  DB_DEBUG("  [HVM] @ %p/%u:", (void*)p, off);
  for (i = 0; i < 8; ++i)
    DB_DEBUG("  %02x", chunk[off + i]);
  DB_DEBUG("\n");
}

static size_t pages_to_mb(size_t pgs) {

  const size_t pages_per_mb = MB(1) / PAGE_SIZE;

  return (pages_per_mb - 1 + pgs) / pages_per_mb;
}

int setup_shadow_allocation(dom_info_t *info) {
  size_t maxmem_mb = pages_to_mb(info->page_num);
  uint32_t mbs = (uint32_t) pages_to_mb
                  ( 256 * info->cpu_num // pages for vcpu structure
                  + maxmem_mb           // pages for P2M map
                  + maxmem_mb           // pages to shadow resident process
                  );

  return set_shadow_allocation(info->dom, mbs);
}

int hvm_create_vm
  (dom_info_t *info
  , db_build_spec *spec
  ) {

  int err;
  size_t i;
  domid_t domid = info->dom;

  // The MFNs for some PFNs of interest.
  xen_pfn_t special_mfns[NR_SPECIAL_PAGES];
  xen_pfn_t last_pfn = 0;

  DB_DEBUG("  [HVM] Setting shadow allocation\n");
  err = setup_shadow_allocation(info);
  if (err != 0) {
    DB_ERROR("  [HVM] Failed to set shadow allocation (%d)\n", err);
    return err;
  }

  DB_DEBUG("  [HVM] Allocating memory\n");
  err = alloc_mem(domid, info->page_num, &last_pfn);
  if (err != 0) return err;

  DB_DEBUG("  [HVM] Loading program headers.\n");
  err = gen_load_phdrs(domid, &info->elf);
  if (err != 0) return err;

  // Allocate special pages, at the end of the IO memory gap.
  DB_DEBUG("  [HVM] Allocate special pages.\n");
  for(i = 0; i < NR_SPECIAL_PAGES; ++i)
    special_mfns[i] = special_pfn(i);

  err = populate_physmap(domid, NR_SPECIAL_PAGES, special_mfns);
  if (err < 0) {
    DB_ERROR("Failed to allocated MFNs for specail pages\n");
    return err;
  }

  if (err < NR_SPECIAL_PAGES) {
    DB_ERROR("Fail. Asked for %d pages, got %d pages\n", NR_SPECIAL_PAGES, err);
    return -ENOMEM;
  }

  DB_DEBUG("  [HVM] Setup start info table\n");
  err = init_start_info(domid, last_pfn);
  if (err != 0) return err;

  DB_DEBUG("  [HVM] Setup identity page tables\n");
  err = identity_page_table(domid);
  if (err != 0) return err;

  DB_DEBUG("  [HVM] Setup entry point\n");
  err = setup_entry_point(domid, &info->elf);
  if (err != 0) return err;

  err = setup_xenstore(domid, spec);
  if (err != 0) return err;

  err = setup_io_pages(domid);
  if (err != 0) return err;

  err = set_param(domid, "PAE",       HVM_PARAM_PAE_ENABLED,  1);
  if (err != 0) return err;

  err = set_param(domid, "viridian",  HVM_PARAM_VIRIDIAN,     0);
  if (err != 0) return err;

  err = set_param(domid, "HPET",      HVM_PARAM_HPET_ENABLED, 0);
  if (err != 0) return err;

  err = set_param(domid, "timer",     HVM_PARAM_TIMER_MODE,
                                      HVMPTM_no_delay_for_missed_ticks);
  if (err != 0) return err;

  err = set_param(domid, "vpt_align", HVM_PARAM_VPT_ALIGN,     1);
  if (err != 0) return err;

  do_test(domid, 0, 0);
  do_test(domid, 0x100, 0);
  do_test(domid, HVM_INFO_PFN, HVM_INFO_OFFSET);

  return 0;
}



