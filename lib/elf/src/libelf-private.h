// BANNERSTART
// Copyright: Xen developers?
// Copyright: 2011 Galois Inc.
// License-file: LICENSE
// BANNEREND

#ifndef __LIBELF_PRIVATE_H__
#define __LIBELF_PRIVATE_H_

#include <byteswap.h>
#include <string.h>
#include <stdlib.h>
#include <smallOS/printk.h>
#include <xen/elfnote.h>
#include <elf-xen/libelf.h>

#define PRIx32 "dx"
#define PRIu32 "du"

#define PRId64 "lld"
#define PRIx64 "llx"
#define PRIu64 "llu"

#define elf_msg(elf, fmt, args ... ) \
   if (elf->verbose) printk(fmt, ## args )
#define elf_err(elf, fmt, args ... ) \
   printk(fmt, ## args )

#define strtoull(str, end, base) simple_strtoull(str, end, base)

#define safe_strcpy(d,s)                        \
do { strncpy((d),(s),sizeof((d))-1);            \
     (d)[sizeof((d))-1] = '\0';                 \
} while (0)

// #endif

#endif /* __LIBELF_PRIVATE_H_ */

