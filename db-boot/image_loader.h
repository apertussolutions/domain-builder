#include <stdint.h>
#include "bzimage.h"

int check_header(void * bzimage, uint64_t size);
int test_elf(void *p, size_t len);
int extract_bzimage(void *bzimage, size_t bzimage_size, void *out, size_t outlen);
