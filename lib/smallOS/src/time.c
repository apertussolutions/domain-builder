//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/time.h>

static inline uint64_t get_rdtsc() {
  register uint64_t tsc_lo asm("rax");
  register uint64_t tsc_hi asm("rdx");

  asm volatile ("rdtsc" ::: "rax", "rdx");

  return tsc_hi << 32 | tsc_lo;
}

// System time (in nano seconds) for a given CPU.
uint64_t nano_cpu_time(volatile vcpu_time_info_t *time) {
  unsigned long sys_time, stamp, nsecs;
  uint32_t version, mult;
  int8_t shift;

  do {
#ifdef CBMC
    int i;
    SMALLLOOP {
      version = time->version; 
    }
#else
    do { version = time->version; } while (version & 1);
#endif
      sys_time = time->system_time;
      stamp    = time->tsc_timestamp;
      shift    = time->tsc_shift;
      mult     = time->tsc_to_system_mul;
  } while(version != time->version);

  nsecs = get_rdtsc();
  nsecs -= stamp;
  if (shift >= 0) nsecs <<= shift; else nsecs >>= -shift;
  nsecs *= mult;
  nsecs >>= 32;
  nsecs += sys_time;
  return nsecs;
}



