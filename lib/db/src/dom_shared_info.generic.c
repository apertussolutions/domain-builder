// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#ifdef ARCH

int setup_shared_info(ARCH)
  ( domid_t dom
  , shared_info_t(ARCH) *shared_info
        // ^ Virtual space to map guest's shared info page.
        //   Gets unmapped after this call.
  , maddr_t *shared_info_ma   // Output
  ) {
  int err;
  mfn_t shared_info_mfn;
  int cpu, i;

  err = get_shared_frame(dom, &shared_info_mfn);
  if (err < 0) return err;

  err = map_foreign_page(dom, shared_info, shared_info_mfn,
                                                  _PAGE_PRESENT | _PAGE_RW);
  if (err < 0) return err;

  for (i = 0; i < sizeof(unsigned long) * 8; ++i) {
    shared_info->evtchn_pending[i] = 0;
    // We mask out everything. The code is written in two steps
    // to avoid a warnings due to varying size of literals.
    shared_info->evtchn_mask[i]    = 0;
    shared_info->evtchn_mask[i]    = ~shared_info->evtchn_mask[i];
  }

  for (cpu = 0; cpu < XEN_LEGACY_MAX_VCPUS; ++cpu) {
     shared_info->vcpu_info[cpu].evtchn_upcall_pending = 0;
     shared_info->vcpu_info[cpu].evtchn_upcall_mask    = 1;
     shared_info->vcpu_info[cpu].evtchn_pending_sel    = 0;
     memset(&shared_info->vcpu_info[cpu].time, 0, sizeof(vcpu_time_info_t));
  }

  *shared_info_ma = mfn_to_maddr(shared_info_mfn);
  return unmap_foreign_page(dom, shared_info);
}

#endif

