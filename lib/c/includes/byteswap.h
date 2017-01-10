// BANNERSTART
// Copyright: 2010-2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND
#ifndef __BYTESWAP_H__
#define __BYTESWAP_H__

#include <stdint.h>


static inline uint16_t bswap_16(uint16_t x)
{
  return (x << 8) | (x >> 8);
}

static inline uint32_t bswap_32(uint32_t x)
{
    return __builtin_bswap32(x);
}

static inline uint64_t bswap_64(uint64_t x)
{
    return __builtin_bswap64(x);
}

#endif
