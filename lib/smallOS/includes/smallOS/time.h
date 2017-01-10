//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_TIME_H__
#define __SMALLOS_TIME_H__

#include <smallOS/xen-version.h>
#include <xen/xen.h>

uint64_t nano_cpu_time(volatile vcpu_time_info_t *time);

static inline
uint64_t second_to_milli(uint64_t s)  { return s * 1000UL; }

static inline
uint64_t second_to_micro(uint64_t s)  { return s * 1000000UL; }

static inline
uint64_t second_to_nano(uint64_t s)   { return s * 1000000000UL; }

static inline
uint64_t milli_to_second(uint64_t s)  { return s / 1000UL; }

static inline
uint64_t micro_to_second(uint64_t s)  { return s / 1000000UL; }

static inline
uint64_t nano_to_second(uint64_t s)   { return s / 1000000000UL; }



#endif

