#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "mod_exp.h"

#undef DEBUG

#ifdef DEBUG
#include "db-log.h"
#endif

static const uint8_t sha256oid[19] =
 {0x30, 0x31, 0x30, 0x0d,
  0x06, 0x09, 0x60, 0x86,
  0x48, 0x01, 0x65, 0x03,
  0x04, 0x02, 0x01, 0x05,
  0x00, 0x04, 0x20};

int assign_big_endian_bytes(unsigned wcnt, uint64_t *w, unsigned bcnt, const uint8_t *b) {
  unsigned i;

  if (bcnt > wcnt * sizeof(*w)) {
    return -1; // b was potentially too large
  }

  set_zero(wcnt, w);

  for (i = 0; i < bcnt; i++) {
    const unsigned w_index = i / 8;
    const unsigned b_index = bcnt - i - 1;

    w[w_index] += (uint64_t)b[b_index] << (8 * (i % 8));
  }

  return 0;
}

int check_decrypted_signature(const uint8_t *oid, size_t oid_len, const uint8_t *hash, size_t hash_len,
                              const uint8_t *signature, size_t signature_len) {
  size_t i = 0;
  
  size_t pad_length = signature_len - hash_len - oid_len - 3;

  int result = 0;

  if (signature_len < hash_len + oid_len + 3) {
    return -2;
  }

  for (i = 0; i < pad_length; i++) {
    if (signature[2 + i] != 0xff) {
      result = -1;
    }
  }

  if (signature[0] != 0 || signature[1] != 1 || signature[2 + pad_length] != 0) {
    result = -1;
  }

  if (memcmp (&signature[3 + pad_length], oid, oid_len) != 0) {
    result = -1;
  }

  if (memcmp (&signature[3 + pad_length + oid_len], hash, hash_len) != 0) {
    result = -1;
  }

  return result;
}

void unpack_bytes(uint8_t *dst, uint64_t *src, size_t src_len) {
  size_t i;
  unsigned j;
  uint64_t temp;
  size_t dst_len = src_len * 8;

  for (i = 0; i < src_len; i++) {
    temp = src[i];
    for (j = 0; j < 8; j++) {
      dst[dst_len - i * 8 - j - 1] = temp;
      temp >>= 8;
    }
  }
}
// Signature verification process

// 1. Verify that public key file hash matches TPM hash
// 2. Parse public key file into modulus and public exponent numbers
// 3. Decrypt signature bytes
// 4. Compare bytes to provided hash bytes.

static
int parse_public_key_file
  (const char *input,
   uint8_t *modulus, size_t *modulus_size,
   uint64_t *exponent)
{
  size_t modulus_used = 0;
  int len;
  int count;
  int keylen;
  char next;

  // Try openssl 1.0 format
  len = sscanf(input, "Public-Key: (%d bit)\nModulus:%n", &keylen, &count);
  if (len != 1) {
    // Try openssl 0.9.8o format
    len = sscanf(input, "Modulus (%d bit):%n", &keylen, &count);
    if (len != 1) {
#ifdef DEBUG
      LOG("[db-server] Failed to parse public key modulus key length: read %d arguments\n", len);
#endif
    }
  }

  input += count;

  len = sscanf(input, " 00:%n", &count);
  input += count;

  do {
    if (modulus_used >= *modulus_size) { 
#ifdef DEBUG
      LOG("[db-server] Parsed modulus exceeds maximum size of %d\n", *modulus_size);
#endif
      return -1;
    }
    len = sscanf(input, "%hhx%c%n", &modulus[modulus_used++], &next, &count); 
    if (len != 2) {
#ifdef DEBUG
      LOG("[db-server] Failed to parse public key modulus: read %d arguments\n", len);
#endif
      return -1;
    }
    input += count;
  } while (len == 2 && next == ':');


  *modulus_size = modulus_used;

  len = sscanf(input, "Exponent: %llu", exponent);
  if (len != 1) {
#ifdef DEBUG
    LOG("[db-server] Failed to parse public key exponent: read %d arguments\n", len);
#endif
    return -1;
  }

  return 0;
}

#define BIGNUMSIZE (1024/64)
int check_signature(const uint8_t * hash, size_t hash_len,
                    const uint8_t * signature, size_t signature_len,
                    const char * public) {

  uint64_t signature_buffer[BIGNUMSIZE] = {0};
  uint64_t decrypted_buffer[BIGNUMSIZE] = {0};
  uint64_t modulus_buffer[BIGNUMSIZE] = {0};
  uint64_t exponent_buffer[BIGNUMSIZE] = {0};

  uint8_t modulus_pre_buffer[1024/8] = {0};
  size_t modulus_pre_used = sizeof(modulus_pre_buffer);
  int err;
  
  err = parse_public_key_file(public, modulus_pre_buffer, &modulus_pre_used, &exponent_buffer[0]);
  if (err != 0) {
#ifdef DEBUG
    LOG("[db-server] Failed to parse public key:\n");
    LOG("  %s\n", public);
#endif
    return err;
  }

  assign_big_endian_bytes(BIGNUMSIZE, modulus_buffer  , modulus_pre_used , modulus_pre_buffer );
  assign_big_endian_bytes(BIGNUMSIZE, signature_buffer, signature_len    , signature          );

  mod_exp(BIGNUMSIZE, decrypted_buffer, signature_buffer, exponent_buffer, modulus_buffer);


  unpack_bytes(modulus_pre_buffer, decrypted_buffer, BIGNUMSIZE);

#ifdef DEBUG
  LOG("Decrypted signature: ");
  for (err = 0; err < sizeof(modulus_pre_buffer); err++) {
    LOG("%02x ", (unsigned int)modulus_pre_buffer[err]);
  }
  LOG("\n");
#endif

  return check_decrypted_signature(sha256oid, sizeof(sha256oid), hash, hash_len,
                              modulus_pre_buffer, sizeof(modulus_pre_buffer));
}


#if 0
int main() {
  uint64_t input[] = {0x0102030405060708, 0x090a0b0c0d0e0fff};
  uint8_t output[8 * 2];
  size_t i;

  unpack_bytes(output, input, 2);

  for (i = 0; i < sizeof(output); i++) {
    printf("%02" PRIx8 " ", output[i]);
  }
   printf("\n");
}
#endif
