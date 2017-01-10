//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/shutdown.h>
#ifndef __SMALLOS_UTILS_H__
#define __SMALLOS_UTILS_H__

#define __in(x) (* ((volatile long *)(x)))


#define xchg(ptr,v) (                             \
  (__typeof__(*(ptr))) __xchg( (unsigned long)(v) \
                             , ptr                \
                             , sizeof(*(ptr))     \
                             )                    \
)

static inline
unsigned long __xchg(unsigned long x, volatile void * ptr, int size)
{
        switch (size) {
                case 1:
                        asm volatile("xchgb %b0,%1"
                                :"=q" (x)
                                :"m" (__in(ptr)), "0" (x)
                                :"memory");
                        break;
                case 2:
                        asm volatile("xchgw %w0,%1"
                                :"=r" (x)
                                :"m" (__in(ptr)), "0" (x)
                                :"memory");
                        break;
                case 4:
                        asm volatile("xchgl %k0,%1"
                                :"=r" (x)
                                :"m" (__in(ptr)), "0" (x)
                                :"memory");
                        break;
                case 8:
                        asm volatile("xchgq %0,%1"
                                :"=r" (x)
                                :"m" (__in(ptr)), "0" (x)
                                :"memory");
                        break;
        }
        return x;
}

/**
 * __ffs - find first bit in word.
 * @word: The word to search
 *
 * Undefined if no bit exists, so code should check against 0 first.
 */
static inline unsigned long __ffs(unsigned long word)
{
  asm volatile (
    "bsfq %1,%0"
    : "=r" (word)
    : "rm" (word)
  );
  return word;
}



static inline
void synch_set_bit(int nr, volatile void * addr) {
  asm volatile (
    "lock btsl %1,%0"
    : "=m" (__in(addr))
    : "Ir" (nr)
    : "memory"
  );
}


static inline
void synch_clear_bit(int nr, volatile void * addr) {
  asm volatile (
    "lock btrl %1,%0"
    : "=m" (__in(addr))
    : "Ir" (nr)
    : "memory"
  );
}


static inline
int synch_const_test_bit(int nr, const volatile void * addr) {
  return ((1UL << (nr & 31)) &
            (((const volatile unsigned int *) addr)[nr >> 5])) != 0;
}

static inline
int synch_var_test_bit(int nr, volatile void * addr) {
  int oldbit;
  asm volatile (
    "btl %2,%1\n\tsbbl %0,%0"
    : "=r" (oldbit)
    : "m" (__in(addr)), "Ir" (nr)
  );
  return oldbit;
}


#define synch_test_bit(nr,addr) \
(__builtin_constant_p(nr) ? \
 synch_const_test_bit((nr),(addr)) : \
 synch_var_test_bit((nr),(addr)))

static inline
int synch_test_and_set_bit(int nr, volatile void * addr) {
    int oldbit;
    asm volatile (
      "lock btsl %2,%1\n\tsbbl %0,%0"
      : "=r" (oldbit), "=m" (__in(addr))
      : "Ir" (nr)
      : "memory"
    );
    return oldbit;
}

/**
 * test_and_clear_bit - Clear a bit and return its old value
 * @nr: Bit to clear
 * @addr: Address to count from
 *
 * This operation is atomic and cannot be reordered.
 * It can be reorderdered on other architectures other than x86.
 * It also implies a memory barrier.
 */
static inline int test_and_clear_bit(int nr, volatile unsigned long * addr) {
  int oldbit;
  asm volatile (
    "btrl %2,%1   \n\t"
    "sbbl %0,%0   \n\t"
    : "=r" (oldbit),"=m" (__in(addr))
    : "Ir" (nr)
    : "memory"
  );
  return oldbit;
}



#endif
