#include<stdbool.h>
#include<stdint.h>
#include <stdio.h>

typedef __int128_t  int128_t;
typedef __uint128_t uint128_t;

/* Word level operations {{{1 */

static inline
unsigned bit_count(unsigned wc) {
  return wc << 6;
}

static inline
bool odd(uint64_t* x) {
  return (x[0] & 1) != 0;
}

static inline
bool is_zero(unsigned wcnt, uint64_t* r) {
  uint64_t* rend = r + wcnt;
  for (; r != rend; ++r) {
    if (*r != 0) return false;
  }
  return true;
}

void set_zero(unsigned wcnt, uint64_t* r) {
  uint64_t* rend = r + wcnt;
  for (; r != rend; ++r) *r = 0;
}

void set_unit(unsigned wcnt, uint64_t* r) {
  unsigned i;

  r[0] = 1;
  for (i = 1; i < wcnt; ++i) {
    r[i] = 0;
  }
}

void assign(unsigned wcnt, uint64_t* r, uint64_t* x) {
  unsigned i;

  for (i = 0; i != wcnt; ++i) {
    r[i] = x[i];
  }
}

uint64_t inc(unsigned wcnt, uint64_t* r, uint64_t* y) {
  unsigned i;

  uint128_t c = 0;
  for (i = 0; i != wcnt; ++i) {
    uint64_t ri = r[i];
    uint64_t yi = y[i];
    c += ((uint128_t) ri) + yi;
    r[i] = (uint64_t) c; c = c  >> 64;
  }
  return (uint64_t) c;
}

int64_t dec(unsigned wcnt, uint64_t* r, uint64_t* y) {
  int64_t b = 0;
  unsigned i;

  for (i = 0; i != wcnt; ++i) {
    uint64_t ri = r[i];
    uint64_t yi = y[i];
    int128_t res = ((int128_t) b) + ((int128_t) ri) - yi;
    r[i] = (uint64_t) res; b = ((int64_t) (res >> 64));
  }
  return b;
}

/* Returns true if x is less than y. */
bool leq(unsigned wcnt, uint64_t* x, uint64_t* y) {
  unsigned i = wcnt;
  while (i != 0) {
    --i;
    if (x[i] != y[i]) return x[i] < y[i];
  }
  return true;
}

/* r and x may alias, but overlapping is unsupported. */
void shr(unsigned wcnt, uint64_t* r, uint64_t carry, uint64_t* x) {
  uint64_t c = carry;
  unsigned i = wcnt;
  while (i != 0) {
    --i;
    uint64_t xi = x[i];
    r[i] = (c << 63) | (xi >> 1);
    c = xi;
  }
}

/* Stores x * y in r (which must contain twice the number of words as cnt). */
void mul(unsigned wcnt, uint64_t* r, uint64_t* x, uint64_t* y) {
  // Initialize r with low-order byte.
  {
    uint128_t d = 0;
    uint64_t x0 = x[0];
    unsigned j;

    for (j = 0; j != wcnt; ++j) {
      d += (((uint128_t) x0) * y[j]);
      r[j] = (uint64_t) d; d = d >> 64;
    }
    r[wcnt] = (uint64_t) d;
  }

  unsigned i;
  for (i = 1; i != wcnt; ++i) {
    uint128_t d = 0;
    uint64_t xi = x[i];
    unsigned ij = i;
    unsigned j;
    for (j = 0; j != wcnt; ++j, ++ij) {
      d += r[ij] + (((uint128_t) xi) * y[j]);
      r[ij] = (uint64_t) d; d = d >> 64;
    }
    r[ij] = (uint64_t) d;
  }
}

#if 0
void print_num(unsigned wcnt, uint64_t* x) {
  unsigned i = wcnt;
  printf("%016llx", x[--i]);
  while (i != 0) {
    printf(" %016llx", x[--i]);
  }
}
#endif

/* Modular arithmetic operations {{{1 */

void mod_inc(unsigned wcnt, uint64_t* r, uint64_t* y, uint64_t* p) { 
  if (inc(wcnt, r, y) != 0 || leq(wcnt, p, r)) dec(wcnt, r, p);
}

void mod_dec(unsigned wcnt, uint64_t* r, uint64_t* y, uint64_t* p) { 
  if (dec(wcnt, r, y) != 0) inc(wcnt, r, p);
}

/** Assigns x = x / 2 (mod p). */
void mod_half(unsigned wcnt, uint64_t* x, uint64_t* p) {
  // If x[0] is odd
  if (odd(x)) {
    int c = inc(wcnt, x, p);
    shr(wcnt, x, c, x);
  } else {
    shr(wcnt, x, 0, x);
  }
}

static uint128_t mask64 = 0xFFFFFFFFFFFFFFFFULL;

static
void mod_red(unsigned wcnt, uint64_t* r, uint64_t c, uint64_t* p) {
  while (c > 10) {
    int128_t b = 0;
    unsigned j;

    for (j = 0; j != wcnt; ++j) {
      // m incrementally stores c * group_order 
      uint128_t m = ((uint128_t) c) * p[j];
      b += ((int128_t) r[j]) - (m & mask64);
      r[j] = (uint64_t) b;
      b = (b >> 64) - (m >> 64);
    }

    c = (uint64_t) (((int128_t) c) + b);
  }
  while (c > 0) {
    c += dec(wcnt, r, p);
  }
}

void mod_mul(unsigned wcnt, uint64_t* r, uint64_t* x, uint64_t* y, uint64_t* p) {
  set_zero(wcnt, r);
  unsigned i = wcnt;
  while (i != 0) {
    --i;

    // Perform shift by 2<<64 followed by reduction.
    {
      uint64_t c = r[wcnt - 1];
      unsigned j;
      for (j = wcnt-1; j != 0; --j) {
        r[j] = r[j-1];
      }
      r[0] = 0;
      mod_red(wcnt, r, c, p);
    }

    // Add in xi * y[j];
    uint128_t xi = x[i];
    {
      uint64_t c = 0;
      unsigned j;
      for (j = 0; j != wcnt; ++j) {
        uint128_t m = xi * y[j];
        uint128_t rj = ((uint128_t) c) + (m & mask64) + r[j];
        r[j] = (uint64_t) rj;
        c = (uint64_t) ((rj >> 64) + (m >> 64));
      }
      mod_red(wcnt, r, c, p);
    }
  }

  while (leq(wcnt, p, r)) dec(wcnt, r, p);
}

void mod_sq(unsigned wcnt, uint64_t* r, uint64_t* x, uint64_t* p) {
  mod_mul(wcnt, r, x, x, p);
}

void sd_mod_exp(unsigned wcnt,
                uint64_t* r,
                uint64_t* s,
                uint64_t* sinv,
                uint64_t* d,
                uint64_t* p,
                uint64_t* h,
                uint64_t* temp) {
  int cnt = bit_count(wcnt);

  shr(wcnt, h, 0, d);
  // If h <- d + (d >> 1) overflows
  if (inc(wcnt, h, d) != 0) {
    assign(wcnt, r, s);
  } else {
    // Otherwise start with r = 1.
    set_unit(wcnt, r);
  }

  unsigned j = cnt;
  while (j != 0) {
    --j;
    unsigned i = j >> 6;

    uint64_t m  = 1L << j;

    uint64_t hi = h[i];
    uint64_t ki = d[i] >> 1;
    if (i + 1 < wcnt) {
      ki |= (d[i+1] & 1) << 63;    
    }

    mod_sq(wcnt, temp, r, p);

    if ((hi & m) != 0 && (ki & m) == 0) {
      mod_mul(wcnt, r, temp, s, p);
    } else if ((hi & m) == 0 && (ki & m) != 0) {
      mod_mul(wcnt, r, temp, sinv, p);
    } else {
      assign(wcnt, r, temp);
    }
  }
}

/**
 * Assigns ra = 1 / y mod p.
 * y, and p are unchanged.
 * Buffers must be distinct from each other as well as x,y, and p.
 * Overwrites t1, t2, and t2 with random data.
 */
void mod_inv(unsigned wcnt,
             uint64_t* ra,
             uint64_t* y,
             uint64_t* p,
             uint64_t* a,
             uint64_t* b,
             uint64_t* rb) {

  assign(  wcnt, a,  p);
  set_zero(wcnt, ra);
  assign(  wcnt, b,  y);
  set_unit(wcnt, rb);

  bool swapped = false;
  while (!is_zero(wcnt, b)) {
    // If odd
    if (odd(b)) {
      if (!leq(wcnt, a, b)) {
        uint64_t* t = a;
        a = b;
        b = t;

        t = ra;
        ra = rb;
        rb = t;
        swapped = !swapped;
      }

      // b <- b - a
      dec(wcnt, b, a);
      // rb <- rb - ra
      mod_dec(wcnt, rb, ra, p);
    }
    // b <- b >> 1
    shr(wcnt, b, 0, b);
    // rb <- half(rb)
    mod_half(wcnt, rb, p);
  }

  if (swapped) assign(wcnt, rb, ra);
}
                
/* Stores b ^ exp (mod p) in r. */ 
void mod_exp(unsigned wcnt, /* Number of 64-bit words in buffers. */
             uint64_t* r,   /* Buffer to store result in. */
             uint64_t* b,   /* Base */
             uint64_t* exp, /* Exponent */
             uint64_t* p    /* Prime modulus */
             ) {
  
  /* Temporary buffers */
  uint64_t t1[wcnt];
  uint64_t t2[wcnt];

  uint64_t binv[wcnt];
  mod_inv(wcnt, binv, b, p, t1, t2, r);
  sd_mod_exp(wcnt, r, b, binv, exp, p, t1, t2);
}

/* Montgomery operations {{{1 */
void mont_coef(unsigned wcnt, uint64_t* r, uint64_t* p) {
  //TODO: Assert cnt is a non-zero multiple of 64.
  // Assert p is odd.
  unsigned cnt = bit_count(wcnt);
  unsigned i;

  uint64_t temp[wcnt];

  // Assign r = 1.
  set_unit(wcnt, r);

  // Assign temp = (p - 1) / 2.
  shr(wcnt, temp, 0, p);

  for (i = 1; i < cnt; ++i) {
    unsigned w = i >> 6;
    unsigned j = i & 0x3F;

    if (odd(temp)) {
      r[w] = r[w] | (((uint64_t) 1) << j);

      uint64_t c = inc(wcnt, temp, p);
      shr(wcnt, temp, c, temp);
    } else {
      shr(wcnt, temp, 0, temp);
    }
  }
}

#if 0
int main(int argc, char** argv) {
  unsigned wcnt = 2;
  uint64_t x[wcnt];
  set_zero(wcnt, x);
  x[0] = 0x3;

  uint64_t p[wcnt];
  p[0] = 0x91;

  uint64_t r[wcnt];

  mod_mul(1, r, x, x, p);
  printf("Result = ");
  print_num(1, r); 
  printf("\n");

  mont_coef(1, r, p);
  printf("Result = ");
  print_num(1, r); 
  printf("\n");

  uint64_t res[2];
  mul(1, res, r, p);
  printf("Test   = ");
  print_num(2, res); 
  printf("\n");

  uint64_t t1[wcnt];
  uint64_t t2[wcnt];
  uint64_t t3[wcnt];

  uint64_t xinv[wcnt];
  mod_inv(wcnt, xinv, x, p, t1, t2, t3);
  printf("xinv   = ");
  print_num(wcnt, xinv); 
  printf("\n");

  mod_mul(wcnt, res, x, xinv, p);
  printf("xinv test = ");
  print_num(wcnt, res); 
  printf("\n");

  uint64_t ex[wcnt];
  set_zero(wcnt, ex);
  ex[0] = 0x5L;

  mod_exp(wcnt, res, x, ex, p);
  printf("Test   = ");
  print_num(wcnt, res); 
  printf("\n");

  return 0;
}
#endif
