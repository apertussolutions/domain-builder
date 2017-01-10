// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#include <smallOS/grant_table.h>
static char dummy_va[PAGE_SIZE] PAGE_ALIGNED RESET_VIRT;

static
int setup_service(ARCH)(dom_info_t *info, vaddr_t(ARCH) addr, db_service_t *s) {
  if (invalid_virt_addr(ARCH)(info, addr)) {
	DB_DEBUG("[DB] Invalid address for service page\n");
	return -1;
  }
  domid_t dom = info->dom;
  vaddr_t(ARCH) vbase = info->elf_params.virt_base;

  mfn_t(ARCH) mfn = get_virt_mfn(ARCH)(dom, vbase, addr);
  if (is_error_mfn(mfn)) {
    DB_DEBUG("[DB] Error getting the virtual MFN %d",mfn_errno(mfn));
    return mfn_errno(mfn);
  }

  evtchn_port_t port;
  int err = bind_unbound_from_to(dom, s->domid, &port);
  if (err != 0) {
     DB_DEBUG("[db] Error setting port for service from %d to %d ret %d\n", 
				dom, s->domid, err);
     return err;
  }
  s->port = port;
  s->mfn  = mfn;
  DB_DEBUG("[DB] Set up service info from %d to %d port %d mfn 0x%x\n",
				dom, s->domid, port, mfn);

  return 0;
}

static
int create_vm_pv(ARCH)
  ( dom0_vga_console_info_t *hw_console
  , dom_info_t *info
  , db_build_spec *spec
  , start_info_t(ARCH) *start_info
  ) {

  int err;
  unsigned long ptab_num;

  vaddr_t(ARCH) phys_map_start, start_info_start, ptabs_start;
  vaddr_t(ARCH) next, blank_mfn_virt;

  const struct elf_binary* elf = &(info->elf);
  const struct elf_dom_parms* parms = &(info->elf_params);

  // Get physical address bounds from elf file.
  vaddr_t(ARCH) virt_min;
  vaddr_t(ARCH) virt_max;
  elf_paddr_bounds(WX)(elf, &virt_min, &virt_max);

  // Interesting addresses
  ASSERT(parms->virt_base >= parms->elf_paddr_offset);
  vaddr_t(ARCH) virt_offset = parms->virt_base - parms->elf_paddr_offset;
  

  // End of virtual address space.
  vaddr_t(ARCH) virt_kend = virt_offset + virt_max;
  // Check for overflow
  ASSERT(virt_kend >= virt_max);

  // Start of ram disk.
  vaddr_t(ARCH) ram_disk_start = PAGE_ALIGN(virt_kend);
  next = ram_disk_start;

  if (spec->ram_disk_size > 0) {
    if (invalid_virt_range(ARCH)(info, ram_disk_start, spec->ram_disk_size)) { 
      DB_DEBUG("Invalid ram disk\n");
      return -1;
    }
  }

  next             += PAGE_ALIGN(spec->ram_disk_size);
  phys_map_start    = next;

  next             += PAGE_ALIGN(info->page_num * sizeof(mfn_t(ARCH)));
  start_info_start  = next;

  // We always leave space to, potentailly, fill-in information
  // about a hradware console.
  next             += PAGE_ALIGN( sizeof(start_info_t(ARCH)) +
                                  sizeof(dom0_vga_console_info_t) );

  blank_mfn_virt    = next;
  next            += 2 * PAGE_SIZE;   // For console & xenstore.

  ptabs_start       = next;

  DB_DEBUG("[DB] Computed the following memory layout:\n");
  DB_DEBUG("       ram-disk:   %p\n", ram_disk_start);
  DB_DEBUG("       PMP:        %p\n", phys_map_start);
  DB_DEBUG("       start info: %p\n", start_info_start);
  DB_DEBUG("       blank MFNs: %p\n", blank_mfn_virt);
  DB_DEBUG("       ptabs_start %p\n", ptabs_start);
  DB_DEBUG("");

  vaddr_t(ARCH) vbase = parms->virt_base;

  // XXX: Check phys_map_start is a valid page.
  DB_DEBUG("[DB] Allocating physical memory (%d pages)\n", info->page_num);
  err = alloc_guest_mfns(ARCH)( info->dom
                              , info->page_num
                              , (phys_map_start - vbase) >> PAGE_BITS
                              );
  if (err != 0) goto error_with_dom;

  DB_DEBUG("[DB] Loading ELF sections:\n");
  err = load_elf_sections(ARCH)(info, view);
  if (err != 0) goto error_with_dom;

  DB_DEBUG("[DB] Loading RAM disk:\n");
  if (spec->ram_disk_size > 0) {
    if (invalid_virt_range(ARCH)(info, ram_disk_start, spec->ram_disk_size)) { 
      err = -1;
      goto error_with_dom;
    }
    DB_DEBUG("   Memory region %p--%p\n   Size %u bytes (%u pages)\n"
            , ram_disk_start
            , ram_disk_start + spec->ram_disk_size
            , spec->ram_disk_size
            , number_of_units(spec->ram_disk_size,PAGE_SIZE)
            );
    err = copy_mem(ARCH)( info
                        , ram_disk_start
                        , spec->ram_disk
                        , spec->ram_disk_size
                        , view
                        );
    if (err != 0) goto error_with_dom;
  } else {
    ram_disk_start = 0;
    DB_DEBUG("  No RAM disk\n");
  }

  DB_DEBUG("[DB] Initializing page tables:\n");
  err = write_page_tables(ARCH)(info, ptabs_start, &ptab_num);
  if (err != 0) { goto error_with_dom; }


  { 
    vaddr_t(ARCH) hypercall = info->elf_params.virt_hypercall;
    if (invalid_virt_addr(ARCH)(info, hypercall)) {
      err = -1;
      goto error_with_dom;
    }

    xen_pfn_t pfn = (hypercall - vbase) >> PAGE_BITS;
    mfn_t(ARCH) mfn = get_pfn_mfn(ARCH)(info->dom, pfn);

    DB_DEBUG("[DB] Setting up the hypercall page:\n");
    DB_DEBUG("  VIR: 0x%p\n", hypercall);
    DB_DEBUG("  PFN: 0x%p\n", pfn);

    if (is_error_mfn(mfn)) { err = -mfn_errno(mfn); goto error_with_dom; }
    DB_DEBUG("  MFN: 0x%p\n", mfn);

    err = hypercall_init(info->dom, mfn);
    if (err != 0) goto error_with_dom;
  }

  if (invalid_virt_addr(ARCH)(info, start_info_start)) {
    err = -1;
    goto error_with_dom;
  }

  //XXX: Check if get_virt_mfn fails.
  DB_DEBUG("[DB] Setting up start info page.\n");
  err = map_foreign_page( info->dom
                        , start_info
                        , get_virt_mfn(ARCH)(info->dom, vbase, start_info_start)
                        , _PAGE_PRESENT | _PAGE_RW
                        );
  if (err != 0) { goto error_with_dom; }

  if (invalid_virt_addr(ARCH)(info, ptabs_start)) { goto error_with_dom; }

  memset(start_info, 0, PAGE_SIZE);
  start_info->mfn_list      = phys_map_start;
  start_info->nr_pages      = info->page_num;
  start_info->mod_start     = ram_disk_start;
  start_info->mod_len       = spec->ram_disk_size;
  start_info->pt_base       = ptabs_start;
  start_info->nr_pt_frames  = ptab_num;
  start_info->flags         = spec->basics->dom_flags;
  // XXX: Are these correct? They seem a bit redundant.
  start_info->first_p2m_pfn = (phys_map_start - vbase) >> PAGE_BITS;
  start_info->nr_p2m_frames = (ptabs_start - phys_map_start) >> PAGE_BITS;
  memset(start_info->cmd_line, 0, MAX_GUEST_CMDLINE);
  if (spec->basics->command_line != NULL)
    strncpy(start_info->cmd_line, spec->basics->command_line, MAX_GUEST_CMDLINE-1);

  DB_DEBUG("[DB] Setting up shared info page.\n");
  maddr_t shared_info_addr;
  err = setup_shared_info(ARCH)
                         ( info->dom
                         , (shared_info_t(ARCH)*) view
                         , &shared_info_addr
                         );
  if (err != 0) goto error_with_dom;

  // NOTE: shared_info_addr is 36 or 64-bit, but the 32-bit version of
  // the start_info field is 32-bit wide, so hopefully,
  // this address is < 4GB in the 32PAE case.
  start_info->shared_info = maddr_num(shared_info_addr);


  DB_DEBUG("[DB] Setting up registers.\n");
  err = setup_registers(ARCH)(
            info,
            start_info_start,
            get_virt_mfn(ARCH)(info->dom, vbase, ptabs_start) * PAGE_SIZE);
  if (err != 0) goto error_with_dom;

  if (spec->console.domid == ~0ULL) {
    DB_DEBUG("[DB] Using hardware console\n");
    start_info->console.dom0.info_off  = sizeof(start_info_t(ARCH));
    start_info->console.dom0.info_size = sizeof(dom0_vga_console_info_t);
    memcpy(start_info + 1, hw_console, start_info->console.dom0.info_size);
  } else {
    DB_DEBUG("[DB] Using virtual console (dom %llu).\n", spec->console.domid);
    err = setup_service(ARCH)(info, blank_mfn_virt + PAGE_SIZE, &spec->console);
    if (err != 0) goto error_with_dom;
    start_info->console.domU.mfn    = spec->console.mfn;
    start_info->console.domU.evtchn = spec->console.port;
  }

  if (spec->xenstore.domid != ~0ULL) {
    DB_DEBUG("[DB] Setting up XenStore (dom %llu).\n", spec->xenstore.domid);
    err = setup_service(ARCH)(info, blank_mfn_virt, &spec->xenstore);
    if (err != 0) goto error_with_dom;
    start_info->store_mfn     = spec->xenstore.mfn;
    start_info->store_evtchn  = spec->xenstore.port;
  } else {
    DB_DEBUG("[DB] No XenStore.\n");
    start_info->store_mfn     = ~0;
    start_info->store_evtchn  = 0;
  }

  if (spec->console.domid != ~0ULL || spec->xenstore.domid != ~0ULL) {
    unsigned long gmfn;
    grant_entry_v1_t *gnttab = (void*)dummy_va;
    DB_DEBUG("[DB] Setting up grant table.\n");
    gmfn = setup_remote_grant_v1(info->dom);
    if (gmfn == -1)
      goto error_with_dom;
    err = map_foreign_page(info->dom, dummy_va, gmfn, _PAGE_PRESENT | _PAGE_RW);
    if (err)
      goto error_with_dom;
    if (spec->console.domid != ~0ULL) {
      gnttab[0].flags = GTF_permit_access;
      gnttab[0].domid = spec->console.domid;
      gnttab[0].frame = spec->console.mfn;
    }
    if (spec->xenstore.domid != ~0ULL) {
      gnttab[1].flags = GTF_permit_access;
      gnttab[1].domid = spec->xenstore.domid;
      gnttab[1].frame = spec->xenstore.mfn;
    }
    unmap_foreign_page(info->dom, dummy_va);
  }

  DB_DEBUG("[DB] VM successfully built.\n");
  return 0;


error_with_dom:
  DB_DEBUG("Failed (%d)\n",err);
  return err;
}
