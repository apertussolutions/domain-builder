#ifndef MODEXP_C
#define MODEXP_C

void set_zero(unsigned wcnt, uint64_t* r);

void mod_exp(unsigned wcnt, /* Number of 64-bit words in buffers. */
             uint64_t* r,   /* Buffer to store result in. */
             uint64_t* b,   /* Base */
             uint64_t* exp, /* Exponent */
             uint64_t* p    /* Prime modulus */
             );

#endif
