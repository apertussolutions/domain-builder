// BANNERSTART
// Copyright: 2011, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include <stdio.h>
#include <string.h>
#include "sha256.h"

size_t str_len(const char * str) {
  size_t i = 0;

  while (str[i] > '\0') i++;

  return i;
}

// Simple test driver for the SHA256 generated by Cryptol
// Test cases are taken from:
//  csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf

// call Cryptol generated sha256 on string given, and 
// print the resulting SHA value
/*@ requires \valid_range(expected,0,31);
  @*/
int test(const char *section, const char *header
        , const char *msg, const uint8_t *expected)
{
  char buffer[SHA256_HASH_SIZE*2+1];
  uint8_t res[SHA256_HASH_SIZE];

/*
  Word32 *res;
  sha256_state_t st;
  size_t j;

  sha256_init(&st);
  i = strlen(msg);
  for (j = 0; j + 100 < i; j+=100) {
    sha256_update(&st, msg + j, 100);
  }
  sha256_update(&st, msg + j, i - j);
  res = sha256_finish(&st);
*/


  buffer[sizeof(buffer)-1] = '\0';

  sha256((uint8_t*)msg, str_len(msg), res);

  printf("\n** Starting test case from Section %s\n", section);
  printf(" sha256 (%s) =\n    ", header);

  
  sha256_show(buffer, res);
  printf("%s\n", buffer);

  sha256_show(buffer, res);
  printf(" expected:\n    %s\n",buffer);

  int ok = memcmp(expected, res, SHA256_HASH_SIZE) == 0;
  printf("\n** Test %s\n", ok ? "Passed" : "Failed");
  return ok;
}

int main(void)
{
  printf("Running SHA256 test vectors from:\n");
  printf(" http://csrc.nist.gov/publications/fips/fips180-2/"
         "fips180-2withchangenotice.pdf\n");
  printf(" Pages: 33-40\n");

  int ok = 1;

  // Test from Section B.1
  uint8_t expected1[] = { 0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23
                        , 0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad };
  ok = test("B.1", "\"abc\"", "abc", expected1) && ok;

  // Test from Section B.2
  uint8_t expected2[] = { 0x24, 0x8d, 0x6a, 0x61, 0xd2, 0x06, 0x38, 0xb8, 0xe5, 0xc0, 0x26, 0x93, 0x0c, 0x3e, 0x60, 0x39
                        , 0xa3, 0x3c, 0xe4, 0x59, 0x64, 0xff, 0x21, 0x67, 0xf6, 0xec, 0xed, 0xd4, 0x19, 0xdb, 0x06, 0xc1 };
  ok = test( "B.2"
            , "\"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq\""
            ,   "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
            , expected2) && ok;

  // Test from Section B.3
  char oneMillionAs[1000001]; // don't forget the extra byte at the end!
  uint8_t expected3[] = { 0xcd, 0xc7, 0x6e, 0x5c, 0x99, 0x14, 0xfb, 0x92, 0x81, 0xa1, 0xc7, 0xe2, 0x84, 0xd7, 0x3e, 0x67
                        , 0xf1, 0x80, 0x9a, 0x48, 0xa4, 0x97, 0x20, 0x0e, 0x04, 0x6d, 0x39, 0xcc, 0xc7, 0x11, 0x2c, 0xd0 };
  size_t i;
  /*@ loop variant sizeof(oneMillionAs) - i;
    @ loop invariant 0 ≤ i < sizeof(oneMillionAs);
    */
  for(i = 0; i < sizeof(oneMillionAs)-1; ++i) oneMillionAs[i] = 'a';
  oneMillionAs[i] = 0;

  ok = test("B.3", "<1 million A's>", oneMillionAs, expected3 ) && ok;

  if (ok) printf("\nAll tests passed.\nDone.\n");
  else printf("\nTests failed.\n");

  return !ok;
}