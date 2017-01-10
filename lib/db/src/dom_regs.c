// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#include <errno.h>
#include "db.h"

#define ARCH x86_64

int setup_registers(ARCH)
  ( dom_info_t *info
  , vaddr_t(ARCH) start_info
  , maddr_t(ARCH) ptab
  ) {
  unsigned cpu;
  vcpu_guest_context_x86_64_t regs;

  memset(&regs,0,sizeof(regs));

  regs.fpu_ctxt.x[0]    = 0x03;
  regs.fpu_ctxt.x[1]    = 0x7F;

  regs.ctrlreg[0]       = 0x80000001UL;

  regs.ctrlreg[3]       = ptab;
  regs.flags            = VGCF_in_kernel_X86_64 | VGCF_online_X86_64;

  regs.user_regs.rsi    = start_info;
  regs.user_regs.rip    = info->elf_params.virt_entry;
  regs.user_regs.rflags = 1 << 9;

  regs.user_regs.cs     = FLAT_KERNEL_CS_X86_64;
  regs.user_regs.ss     = FLAT_KERNEL_SS_X86_64;
  regs.user_regs.es     = FLAT_KERNEL_DS_X86_64;
  regs.user_regs.ds     = FLAT_KERNEL_DS_X86_64;
  regs.user_regs.fs     = FLAT_KERNEL_DS_X86_64;
  regs.user_regs.gs     = FLAT_KERNEL_DS_X86_64;

  for (cpu = 0; cpu < info->cpu_num; ++cpu) {
    int err = set_vcpu_context(info->dom, cpu, &regs);
    if (err != 0) {
      DB_DEBUG("[DB] Failed to set CPU[%u] context (%d)\n", cpu, err);
      return err;
    }
  }
  return 0;
}

#undef ARCH


#define ARCH x86_32pae

int setup_registers(ARCH)
  ( dom_info_t *info
  , vaddr_t(ARCH) start_info
  , maddr_t(ARCH) ptab
  ) {
  unsigned cpu;
  vcpu_guest_context_x86_32_t regs;

  memset(&regs,0,sizeof(regs));

  regs.fpu_ctxt.x[0]    = 0x03;
  regs.fpu_ctxt.x[1]    = 0x7F;

  regs.ctrlreg[0]       = 0x80000001;

  DB_DEBUG("[DB] Root ptab MFN: %p\n", ptab);

  // High bits go into low 12 bits ("extended cr3" encoding)
  regs.ctrlreg[3]       = ((unsigned)(ptab) | ((unsigned)(ptab >> 32)));

  regs.flags            = VGCF_in_kernel_X86_32 | VGCF_online_X86_32;

  regs.user_regs.esi    = start_info;
  regs.user_regs.eip    = info->elf_params.virt_entry;
  regs.user_regs.eflags = 1 << 9;

  regs.user_regs.cs     = FLAT_KERNEL_CS_X86_32;
  regs.user_regs.ss     = FLAT_KERNEL_SS_X86_32;
  regs.user_regs.es     = FLAT_KERNEL_DS_X86_32;
  regs.user_regs.ds     = FLAT_KERNEL_DS_X86_32;
  regs.user_regs.fs     = FLAT_KERNEL_DS_X86_32;
  regs.user_regs.gs     = FLAT_KERNEL_DS_X86_32;

  for (cpu = 0; cpu < info->cpu_num; ++cpu) {
    int err = set_vcpu_context(info->dom, cpu, &regs);
    if (err != 0) return err;
  }
  return 0;
}



