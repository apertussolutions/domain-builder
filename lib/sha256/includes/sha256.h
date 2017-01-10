// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#ifndef __sha256_H_INCLUDED__
#define __sha256_H_INCLUDED__

#include "BV.h"

#define SHA256_HASH_SIZE 32

typedef Word32 Block[16];

typedef struct {
  uint32_t H[8];
  uint8_t buffer[sizeof(Block)];
  size_t buffer_size;
  size_t total_size;
} sha256_state_t;

/*@ predicate good_state(sha256_state_t *s)
      = \valid(s)
      ∧ s->buffer_size < 64
      ∧ \valid_range(s->H, 0, 7)
      ∧ \valid_range(s->buffer, 0, 63);
  @*/




/*@ requires \valid_range(msg,0,len-1)
           ∧ \valid_range(res,0,63);
  */
void sha256 (const uint8_t *msg, size_t len, uint8_t *res);


/*@ requires \valid(st);
    ensures st->buffer_size ≡ 0
          ∧ st->total_size  ≡ 0;
   */
void sha256_init (sha256_state_t *st);


/*@ requires good_state(st)
           ∧ st->total_size + len ≤ 0xffffffffffffffff
           ∧ \valid_range(msg,0,len-1);

    ensures good_state(st)
          ∧ st->total_size ≡ \old(st->total_size) + len;

   */
void sha256_update (sha256_state_t *st, const uint8_t *msg, size_t len);


/*@ requires good_state(st);
  @ requires \valid_range(res,0,31);
  @*/
void sha256_finish (sha256_state_t *st, uint8_t *res);

/*@ requires \valid_range(dst,0,31)
  @        ∧ \valid_range(str,0,63);
  @*/
int sha256_parse(uint8_t *dst, const char *str);


/*@ requires \valid_range(dst,0,63) 
  @        ∧ \valid_range(src,0,31);
  @*/
void sha256_show(char *dst, const uint8_t *src);
#endif
