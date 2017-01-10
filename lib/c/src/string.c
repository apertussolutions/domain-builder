// BANNERSTART
// Copyright: 2003 - Rolf Neugebauer - Intel Research Cambridge
// Copyright: 2011 - Galois, Inc.
// License-file: miniOS-LICENSE
// Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
// Author: Eric Mertens    (emertens@galois.com)
// BANNEREND


#include <string.h>

int memcmp(const void * cs,const void * ct,size_t count)
{
        size_t i;

        /*@ loop variant count - i;
          @ loop invariant ∀ ℤ j; 0 ≤ j < i ⇒ ((char*)cs)[j] ≡ ((char*)ct)[j];
          @*/
	for (i = 0; i < count && ((char*)cs)[i] == ((char*)ct)[i]; i++) {}
        if (i < count) {
          if (((char*)cs)[i] < ((char*)ct)[i]) return -1;
          else return 1;
        } else {
          return 0;
        }
}

void * memcpy(void * dest,const void *src,size_t count)
{
	char *tmp = (char *) dest;
        const char *s = src;
        size_t i;

        /*@ loop variant count - i;
          @ loop invariant ∀ ℤ j; 0 ≤ j < i ⇒ tmp[j] ≡ s[j];
          @*/
	for (i = 0; i < count; i++)
          tmp[i] = s[i];

	return dest;
}

int strncmp(const char * cs,const char * ct,size_t count)
{
	register signed char __res = 0;

	while (count) {
		if ((__res = *cs - *ct++) != 0 || !*cs++) break;
		count--;
	}

	return __res;
}

int strcmp(const char * cs,const char * ct)
{
  size_t i = 0;

  /*@ loop variant strlen(ct) - i;
    @ loop invariant valid_string(ct + i);
    @ loop invariant valid_string(cs + i);
    @ loop invariant i ≤ strlen(ct);
    @ loop invariant i ≤ strlen(cs);
    @ loop invariant ∀ ℤ j; 0 ≤ j < i ⇒ cs[j] ≡ ct[j];
    @*/
  for (i = 0; cs[i] && ct[i] && cs[i] == ct[i]; i++) {
  }

  if (cs[i] == ct[i]) return 0;
  if (cs[i] < ct[i])  return -1;
  return 1;
}

char * strcpy(char * dest,const char *src)
{
  size_t i;

  /*@ loop variant strlen(src) - i;
    @ loop invariant i <= strlen(src);
    @ loop invariant ∀ ℤ k; 0 ≤ k < i ⇒ dest[k] ≢ 0;
    @ loop invariant valid_string(&src[i]);
    @*/
  for (i = 0; src[i]; ++i) {
    dest[i] = src[i];
  }
  //@ assert i == strlen(src);
  //@ assert ∀ ℤ k; 0 ≤ k < i ⇒ dest[k] ≢ 0;

  dest[i] = '\0';
  //@ assert strlen(dest) == i;
  return dest;
}

char * strncpy(char * dest,const char *src,size_t count)
{
        size_t i;

        /*@ loop variant count - i;
          @ loop invariant i ≤ strlen(src);
          */
        for (i = 0; i < count && '\0' != src[i]; i++) {
          dest[i] = src[i];
        }

        /*@ loop variant count - i;
         */
  for (; i < count; i++)
          dest[i] = '\0';

        return dest;
}

void * memset(void * s,int c,size_t n)
{
        char * xs = s;
        size_t i;

        /*@ loop variant n - i;
          @ loop invariant ∀ ℤ k; 0 ≤ k < i ⇒ ((char*)s)[k] ≡ c;
          @*/
        for (i = 0; i < n; ++i)
           xs[i] = c;

        return s;
}

size_t strnlen(const char * s, size_t count)
{
  size_t i;

  /*@ loop variant count - i;
    @ loop invariant i ≤ strlen(s);
    @ loop invariant i ≤ count;
    @ loop invariant ∀ ℤ k; 0 ≤ k < i ⇒ s[k] ≢ '\0';
    @*/
  for (i = 0; i < count && '\0' != s[i]; i++) {}

  return i;
}

char * strcat(char * dest, const char * src)
{
    size_t out, in;

    /*@ loop variant strlen(dest) - out;
      @ loop invariant out < 0xffffffffffffffff;
      @ loop invariant out ≤ strlen(dest);
      @ loop invariant ∀ ℤ k; 0 ≤ k < out ⇒ dest[k] ≢ '\0';
      @*/
    for (out = 0; dest[out]; out++) { }

    /*@ loop variant strlen(src) - in;
      @ loop invariant in ≤ strlen(src);
      @ loop invariant in + out < 0xffffffffffffffff;
      @ loop invariant ∀ ℤ k; 0 ≤ k < (out + in) ⇒ dest[k] ≢ '\0';
      @*/
    for (in = 0; src[in]; ++in, ++out) {
      dest[out+in] = src[in];
    }

    dest[out+in] = '\0';

    return dest;
}

size_t strlen(const char * s)
{
  size_t i;

  /*@ loop invariant i ≤ strlen(s);
    @ loop invariant ∀ ℤ k; 0 ≤ k < i ⇒ s[k] ≢ '\0';
    @ loop variant strlen(s) - i;
    @*/
  for(i = 0; '\0' != s[i]; i++) { }

  return i;
}

/*@ requires valid_string(s);
  @ requires -128 ≤ c < 0 ∨ 0 < c ≤ 127;
  @
  @ assigns \nothing;
  @
  @ behavior success:
  @  assumes ∃ ℤ k; 0 ≤ k < strlen(s) ∧ s[k] ≡ c;
  @  ensures *\result ≡ c
  @        ∧ valid_string(\result);
  @
  @ behavior failure:
  @  assumes ∀ ℤ i; 0 ≤ i < strlen(s) ⇒ s[i] ≢ c;
  @  ensures \result ≡ \null;
  @*/
char * strchr(const char * s, int c)
{
        /*@ loop variant strlen(s);
          @ loop invariant valid_string(s);
          @ for success:
          @  loop invariant ∃ ℤ k; 0 ≤ k < strlen(s) ∧ s[k] ≡ c;
          @ for failure:
          @   loop invariant ∀ ℤ i; 0 ≤ i < strlen(s) ⇒ s[i] ≢ c;
          @*/
	for(; *s != (char) c; ++s)
           if (*s == '\0') return NULL;
        return (char *)s;
}

char * strrchr(const char * s, int c)
{
        const char *res = NULL;
        for(; *s != '\0'; ++s)
          if (*s == (char) c)
            res = s;
        return (char *)res;
}

char * strstr(const char * s1,const char * s2)
{
        size_t l1, l2;

        l2 = strlen(s2);
        if (!l2)
                return (char *) s1;
        l1 = strlen(s1);
        while (l1 >= l2) {
                l1--;
                if (!memcmp(s1,s2,l2))
                        return (char *) s1;
                s1++;
        }
        return NULL;
}

void *memchr(const void *s, int c, size_t n) {
  size_t i;

  for (i = 0; i < n; i++) {
    if ( ((unsigned char*)s)[i] == (unsigned char) c) {
      return (void *)s + i;
    }
  }
  return NULL;
}

