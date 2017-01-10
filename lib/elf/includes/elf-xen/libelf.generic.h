inline static
const ElfPhdr(WX)* elf_phdr_by_index(WX)(const struct elf_binary *elf, int index) {
  return (const ElfPhdr(WX)*) (elf->phdr_start + elf->phdr_size * index);
}

inline static
int elf_phdr_is_loadable(WX)(const ElfPhdr(WX)* phdr) {
  uint64_t p_type = phdr->p_type;
  uint64_t p_flags = phdr->p_flags;
  return (p_type == PT_LOAD) && (p_flags & (PF_W | PF_X)) != 0;
}

/**
 * Returns minimum and maxium memory addresses defined by loadable elf program
 * entry.
 * N.B. Assumes that check_phdr_entries has already been called to validate the
 * elf file.
 */
inline static
void elf_paddr_bounds(WX)(const struct elf_binary* elf,
                          ElfAddr(WX)* min_res,
                          ElfAddr(WX)* max_res) {
  uint16_t i;
  uint16_t cnt = elf->phdr_count;
  ElfAddr(WX) min = -1;
  ElfAddr(WX) max = 0;
  for (i = 0; i != cnt; ++i) {
    const ElfPhdr(WX)* phdr = elf_phdr_by_index(WX)(elf, i);
    if (elf_phdr_is_loadable(WX)(phdr)) {
      ElfAddr(WX) paddr = phdr->p_paddr;
      ElfAddr(WX) end = paddr + phdr->p_memsz;
      // Check for overflow (only possible if elf has not been checked).
      ASSERT(paddr <= end);
      if (min > paddr) min = paddr;
      if (max < end) max = end;
    }
  }
  *min_res = min;
  *max_res = max;
}
