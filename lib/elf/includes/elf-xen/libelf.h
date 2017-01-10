/******************************************************************************
 * libelf.h
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifndef __XEN_LIBELF_H__
#define __XEN_LIBELF_H__

#ifdef __cplusplus
extern "C" {
#endif 

#if defined(__i386__) || defined(__x86_64__) || defined(__ia64__)
#define XEN_ELF_LITTLE_ENDIAN
#else
#error define architectural endianness
#endif

#include <stddef.h>
#include <stdint.h>

#include "elfstructs.h"
#include <xen/elfnote.h>
#include <xen/features.h>

/* ------------------------------------------------------------------------ */

/** Data extracted from Elf binary */
struct elf_binary {
    /* elf binary */
    const char* image;

    char elf_class;
    char data;

    int verbose;

    /** Number of phdr entries in elf. */
    uint16_t phdr_count;
    /** Points to start of phdr entries. */
    const char* phdr_start;
    /** Size of each phdr. */
    uint64_t phdr_size;

    /** Number of shdr entries in elf. */
    uint16_t shdr_count;
    /** Points to start of shdr entries. */
    const char* shdr_start;
    /** Size of each shdr. */
    uint64_t shdr_size;

    /** Entry address for elf binary. */
    uint64_t entry;

    /** Points to start of string table. */
    const char* strtab_start;
    /** Stores size of string table. */
    uint64_t strtab_size;
};

#define ELF_POLY(x,y)            x##y
#define ELF_TYPE(pre,wx,suf)     pre##wx##_##suf

#define elf_phdr_count(wx)       ELF_POLY(elf_phdr_count,wx)
#define elf_phdr_by_index(wx)    ELF_POLY(elf_phdr_by_index,wx)
#define elf_phdr_is_loadable(wx) ELF_POLY(elf_phdr_is_loadable,wx)

#define elf_paddr_bounds(wx)     ELF_POLY(elf_paddr_bounds,wx)

#define ElfAddr(wx) ELF_TYPE(uint, wx, t)
#define ElfEhdr(wx) ELF_TYPE(Elf, wx, Ehdr)
#define ElfPhdr(wx) ELF_TYPE(Elf, wx, Phdr)
#define ElfShdr(wx) ELF_TYPE(Elf, wx, Shdr)

#define ASSERT(b)

#define WX 32
#include "elf-xen/libelf.generic.h"
#undef WX

#define WX 64
#include "elf-xen/libelf.generic.h"
#undef WX

/* ------------------------------------------------------------------------ */
/* accessing elf header fields                                              */

#define elf_32bit(elf) (ELFCLASS32 == (elf)->elf_class)
#define elf_64bit(elf) (ELFCLASS64 == (elf)->elf_class)

/* ------------------------------------------------------------------------ */
/* xc_libelf_tools.c                                                        */

/* ------------------------------------------------------------------------ */
/* xc_libelf_loader.c                                                       */

int elf_init(struct elf_binary* elf, const char* image, size_t size);
void elf_set_verbose(struct elf_binary* elf);

/* ------------------------------------------------------------------------ */
/* xc_libelf_dominfo.c                                                      */

#define UNSET_ADDR          ((uint64_t)-1)

enum xen_dom_parms_status {
    XEN_PARMS_UNINITIALIZED, // Parameters were not initialized.
    XEN_PARMS_ELF_NOTES, // Parameters were initialized from ELF notes
    XEN_PARMS_XEN_GUEST // Parameters were initialized from legacy __xen_guest section.
};

/*** Domain parameters for Xen domain. */
struct elf_dom_parms {
    enum xen_dom_parms_status status;

    /* parsed */
    char loader[16];
    char guest_os[16];
    char guest_ver[16];
    char xen_ver[16];
    int pae;
    uint64_t virt_base;
    uint64_t virt_entry;
    uint64_t virt_hypercall;
    uint64_t virt_hv_start_low;
    uint64_t p2m_base;
    uint64_t elf_paddr_offset;
    uint32_t f_supported[XENFEAT_NR_SUBMAPS];
    uint32_t f_required[XENFEAT_NR_SUBMAPS];
};

static inline void elf_xen_feature_set(int nr, uint32_t * addr) {
    addr[nr >> 5] |= 1 << (nr & 31);
}

/**
 * Parse domain parameters from elf binary, including error checking.
 * Returns 0 if successful and negative number if there is an error.
 */
int elf_parse_dom_parms(const struct elf_binary* elf,
                        struct elf_dom_parms* parms);
#ifdef __cplusplus
}
#endif 

#endif /* __XEN_LIBELF_H__ */
