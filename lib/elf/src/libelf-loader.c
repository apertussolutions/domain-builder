// BANNERSTART
// Copyright: Xen developers?
// Copyright: 2011 Galois Inc.
// License-file: LICENSE
// BANNEREND

/*
 * parse and load elf binaries
 */

#include "libelf-private.h"

/* ------------------------------------------------------------------------ */

#define elf_check(wx) ELF_POLY(elf_check,wx)

#define WX 64
#include "libelf-loader.generic"
#undef WX

#define WX 32
#include "libelf-loader.generic"
#undef WX

int elf_init(struct elf_binary *elf, const char *image, size_t size) {
  int is_elf = size >= EI_NIDENT
            && image[0] == ELFMAG0
            && image[1] == ELFMAG1
            && image[2] == ELFMAG2
            && image[3] == ELFMAG3;
  if (!is_elf) {
      elf_err(elf, "%s: not an ELF binary\n", __FUNCTION__);
      return -1;
  }

  unsigned char elf_class = image[EI_CLASS];
  if (elf_class != ELFCLASS32 && elf_class != ELFCLASS64) {
    elf_err(elf, "%s: not an 32 or 64bit ELF binary\n", __FUNCTION__);
    return -1;
  }

  unsigned char elf_data = image[EI_DATA];
  if (elf_data != ELFDATA2LSB) {
    elf_err(elf, "%s: data is not least-significant byte first\n", __FUNCTION__);
    return -1;
  }

  elf->image = image;
  elf->elf_class = elf_class;
  elf->data = elf_data;
  elf->verbose = 0;

  // Check elf file and initialize computed addresses.
  if (elf_class == ELFCLASS64) {
    int err = elf_check(64)(image, size, elf);
    if (err) return err;
  } else {
    int err = elf_check(32)(image, size, elf);
    if (err) return err;
  }

  return 0;
}

void elf_set_verbose(struct elf_binary *elf) {
  elf->verbose = 1;
}
