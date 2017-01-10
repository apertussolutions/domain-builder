// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

/* Wrapper code to go with the Cryptol
   generated SHA256 block function.
*/

#include "BV.h"
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "block.c"

#include "sha256.h"
#include "lemmas.h"

/*@ requires \valid_range(dest,0,n-1);
    requires \valid_range(source,0,n-1);
  @*/
static void memcopy(uint8_t * dest, const uint8_t * source, size_t n) {
  size_t i;
  //@ loop variant n - i;
#ifdef CBMC
  SMALLLOOP {
#else
  for (i = 0; i < n; i++) {
#endif
    dest[i] = source[i];
  }
  return;
}

/*@ requires \valid_range(dest,0,n-1);
  @*/
static void mem_set(uint8_t * dest, uint8_t e, size_t n) {
  size_t i;
  //@ loop variant n - i;
  for (i = 0; i < n; i++) {
    dest[i] = e;
  }
  return;
}

static inline unsigned int SW(unsigned int x) {
  return __builtin_bswap32(x);
}

static inline unsigned long long SW64(unsigned long long x){
  return __builtin_bswap64(x);
}

/*@ requires \valid_range(in,0,63);
    requires \valid_range(h,0,7);
  @*/
static void addBlock(const uint8_t * in, uint32_t *h) {

   uint32_t *m = (uint32_t*)in;
   uint32_t o[8];
   size_t i;
   
   block( h[ 0], h[ 1], h[ 2], h[ 3], h[ 4], h[ 5], h[ 6], h[ 7]
        , SW(m[ 0]), SW(m[ 1]), SW(m[ 2]), SW(m[ 3]), SW(m[ 4]), SW(m[ 5]), SW(m[ 6]), SW(m[ 7])
        , SW(m[ 8]), SW(m[ 9]), SW(m[10]), SW(m[11]), SW(m[12]), SW(m[13]), SW(m[14]), SW(m[15])
        // collect the output
        , &o[0], &o[1], &o[2], &o[3]
        , &o[4], &o[5], &o[6], &o[7] );

   /*@ loop variant 8 - i; @*/
#ifdef CBMC
   for (i = 0; i < 1; i++) {
#else
   for (i = 0; i < 8; i++) {
#endif
     h[i] = o[i];
   }
}

void sha256_init(sha256_state_t *state) {
  state->H[0] = 0x6a09e667U;
  state->H[1] = 0xbb67ae85U;
  state->H[2] = 0x3c6ef372U;
  state->H[3] = 0xa54ff53aU;
  state->H[4] = 0x510e527fU;
  state->H[5] = 0x9b05688cU;
  state->H[6] = 0x1f83d9abU;
  state->H[7] = 0x5be0cd19U;
  state->buffer_size = 0;
  state->total_size = 0;
}


void sha256_update(sha256_state_t *state, const uint8_t * msg, size_t len) {

  int remain = sizeof(state->buffer) - state->buffer_size;
  size_t i;

  state->total_size += len;

  if (len < remain) {
    // If there is not enough to make a whole block, save for later

    /*@ loop variant len - i;
      @*/
#ifdef CBMC
    SMALLLOOP
#else
    for (i = 0; i < len; i++)
#endif
      state->buffer[state->buffer_size+i] = msg[i];

    state->buffer_size += len;

  } else {

    if (state->buffer_size > 0) {
      // If some data is already in the buffer, use that first

      memcopy(&state->buffer[state->buffer_size], msg, remain);
      msg += remain;
      len -= remain;

      addBlock(state->buffer, state->H);
    }

    /*@ loop variant len;
      @ loop invariant \valid_range(msg,0,len-1); @*/
#ifdef CBMC
    for(len = 8; len >= 0; len--) {
#else
    while (len >= sizeof(Block)) {
#endif
      // Add all of the whole blocks from the input data

      addBlock(msg, state->H);
      msg += sizeof(Block);
#ifndef CBMC
      len -= sizeof(Block);
#endif
    }

    // Save the remaining bytes in the buffer for next time
    memcopy(state->buffer, msg, len);
    state->buffer_size = len;
  }
}


void sha256_finish(sha256_state_t *state, uint8_t *out) {
  const uint8_t terminator = 0x80;
  const uint8_t pad = 0;
  size_t i;

  size_t remain;

  // If the buffer was full it would have been handled already
  state->buffer[state->buffer_size++] = terminator;

  remain = sizeof(state->buffer) - state->buffer_size;

  if (remain < 8) {
    // If there isn't enough space, flush out this buffer and start a new one
    mem_set(state->buffer + state->buffer_size, pad, remain);

    addBlock(state->buffer, state->H);

    state->buffer_size = 0;
    remain = sizeof(state->buffer);
  }

  mem_set(state->buffer + state->buffer_size, pad, remain - sizeof(unsigned long long));

  unsigned long long bitCount = (unsigned long long)state->total_size * 8;

  state->buffer[56] = bitCount >> 56;
  state->buffer[57] = bitCount >> 48;
  state->buffer[58] = bitCount >> 40;
  state->buffer[59] = bitCount >> 32;
  state->buffer[60] = bitCount >> 24;
  state->buffer[61] = bitCount >> 16;
  state->buffer[62] = bitCount >> 8;
  state->buffer[63] = bitCount;

  addBlock(state->buffer, state->H);
  state->buffer_size = 0;

  // Our hashing code packs 4-byte arrays into 32-bit words like this:
  // LSB [ 3 | 2 | 1 | 0 ] MSB

  /*@ loop variant SHA256_HASH_SIZE / sizeof(uint32_t) - i; @*/
  for (i = 0; i < SHA256_HASH_SIZE / sizeof(uint32_t); ++i) {
      *(uint32_t*)(out + 4*i) = SW(state->H[i]);
  }
}

void sha256(const uint8_t * msg, size_t len, uint8_t * out) {
  sha256_state_t st;

  sha256_init(&st);
  sha256_update(&st, msg, len);
  sha256_finish(&st, out);
}

static
char digit_to_hex(uint8_t hex_digit) {
  const char digits[16] = "0123456789abcdef";

  if (hex_digit < 16) return digits[hex_digit];
  else return '?';
}

/*@ ensures \result < 16 || \result == 255;
  @*/
static
uint8_t hex_to_digit(char c) {
  const char digits[16] = "0123456789abcdef";
  const char digitsU[16] = "0123456789ABCDEF";
  size_t i;

  //@ loop variant 16-i;
  for (i = 0; i < 16; i++) {
    if (digits[i] == c || digitsU[i] == c) return i;
  }
  return 0xff;
}


// Expects (2 * SHA256_HASH_SIZE) characters, representing a SHA256 hash in hex.
// The destination should have space for SHA256_HASH_SIZE bytes.
int sha256_parse(uint8_t *dst, const char *src) {
  size_t i;

  //@ loop variant SHA256_HASH_SIZE - i;
  for (i = 0; i < SHA256_HASH_SIZE; ++i) {
    uint8_t val_hi, val_lo;

    val_hi = hex_to_digit(src[i * 2]);
    if (val_hi == 0xff) return -1;
    val_lo = hex_to_digit(src[i * 2 + 1]);
    if (val_lo == 0xff) return -1;

    dst[i] = (val_hi << 4) + val_lo;
  }
  return 0;
}

void sha256_show(char *dst, const uint8_t *src) {
  size_t i;

  //@ loop variant SHA256_HASH_SIZE - i;
  for (i = 0; i < SHA256_HASH_SIZE; ++i) {
    dst[2*i]   = digit_to_hex(src[i] >> 4);
    dst[2*i+1] = digit_to_hex(src[i] & 0xf);
  }
}


