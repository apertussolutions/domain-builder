static
int load_phdrs(WX)(domid_t domid, const struct elf_binary *elf) {
  uint16_t count = elf->phdr_count;
  uint16_t i;
  for (i = 0; i < count; ++i) {
    const ElfPhdr(WX)* phdr = elf_phdr_by_index(WX)(elf, i);

    if (!elf_phdr_is_loadable(WX)(phdr)) continue;

    uint64_t paddr  = phdr->p_paddr;
    uint64_t memsz  = phdr->p_memsz;
    uint64_t offset = phdr->p_offset;
    uint64_t filesz = phdr->p_filesz;

    if (memsz < filesz) return -EINVAL;

    int err;
    err = foreign_memcpy(domid, paddr, elf->image + offset, filesz);
    if (err != 0) return err;

    err = foreign_memzero(domid, paddr + filesz, memsz - filesz);
    if (err != 0) return err;
  }

  return 0;
}
