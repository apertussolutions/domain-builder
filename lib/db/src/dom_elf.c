// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND


/*
 * This module contains functionality for processing ELF files.
 * We perform some rudimentary validation on the ELF header,
 * identify important information about the virtual machine
 * (such as the location of the inital instruction, for example),
 * and load the different ELF sections at their appropriate locations.
 *
 */

#include "db.h"
#include <elf-xen/libelf.h>
#include <errno.h>


// Returns the number of the virtual page after the last loaded ELF section.
int load_elf
  ( dom_info_t *info    // Partial info about the domain
  , uint64_t is_hvm
  , uint64_t image_size
  , void*    image
  , uint8_t  *view
  ) {

  struct elf_binary *elf = &(info->elf);
  struct elf_dom_parms* params = (void*)&(info->elf_params);

  int err = elf_init(elf, image, image_size);
  if (err < 0) return err;

  elf_set_verbose(elf);

  if (!is_hvm) {
    err = elf_parse_dom_parms(elf, params);
    if (err < 0) return err;
  }

  // Check compatibility
  if (elf_64bit(elf)) {
    info->guest_type = Guest64;
  } else
  if (elf_32bit(elf) /*&& params.pae == 3*/ /* bimodal, see libelf */) {
    info->guest_type = Guest32pae;
  } else {
    DB_ERROR("  [ELF] Unsupported guest\n");
    return -EINVAL;
  }
  return 0;
}

#define ARCH x86_64
#define ARCH_SIZE 64
#include "dom_elf.generic.c"
#undef ARCH_SIZE
#undef ARCH

#define ARCH x86_32pae
#define ARCH_SIZE 32
#include "dom_elf.generic.c"
#undef ARCH_SIZE
#undef ARCH
