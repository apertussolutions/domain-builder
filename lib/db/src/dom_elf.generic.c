// BANNERSTART
// - Copyright 2008, Galois, Inc.
// - U.S. Government Proprietary, For Official Use Only
// Author: Iavor S. Diatchki <diatchki@galois.com>
// BANNEREND

#ifdef ARCH
#define ELF_PASTER(x,y,z) x ## y ## _ ## z
#define ELF_EVALUATOR(x,y,z) ELF_PASTER(x,y,z)

#define ElfType(suffix) ELF_EVALUATOR(Elf, ARCH_SIZE, suffix)
#define WX ARCH_SIZE

// Modified version of "elf_load_binary".
int load_elf_sections(ARCH)
  ( dom_info_t *info // Partial info about the domain
  , uint8_t *view
  ) {
    struct elf_binary *elf = &(info->elf);
    const struct elf_dom_parms* parms = &(info->elf_params);

    vaddr_t(ARCH) virt_offset = parms->virt_base - parms->elf_paddr_offset;

    uint64_t count = elf->phdr_count;
    uint64_t i;
    for ( i = 0; i < count; i++ ) {
        const ElfType(Phdr)* phdr = elf_phdr_by_index(WX)(elf, i);
        if (!elf_phdr_is_loadable(WX)(phdr))
            continue;
        uint64_t filesz = phdr->p_filesz;
        uint64_t memsz = phdr->p_memsz;
        ASSERT(filesz < memsz);

        uint8_t* src = (uint8_t*) elf->image + phdr->p_offset;
        vaddr_t(ARCH) dest = (vaddr_t(ARCH)) (virt_offset + phdr->p_paddr);

        // N.B. We have already checked that file size is valid in elf library.
        // See definition of elf_check
        
        // Check that destination is valid address.
        if (invalid_virt_range(ARCH)(info, dest, memsz)) { 
          DB_DEBUG("Invalid elf destination");
          return -1;
        }
        
        int err = copy_mem(ARCH)(info, dest, src, filesz, view);
        if (err != 0) {
          DB_DEBUG("Failed to copy mem\n");
          return err;
        }

        err = fill_mem(ARCH)(info, dest + filesz, 0, memsz - filesz, view);
        if (err != 0) {
          DB_DEBUG("Failed to fill mem\n");
          return err;
        }
    }

    return 0;
}
#undef WX
#undef ElfType
#undef ELF_EVALUATOR
#undef ELF_PASTER

#endif
