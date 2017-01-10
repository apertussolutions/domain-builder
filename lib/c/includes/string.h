/* -*-  Mode:C; c-basic-offset:4; tab-width:4 -*-
 ****************************************************************************
 * (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
 ****************************************************************************
 *
 *        File: string.h
 *      Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
 *     Changes: 
 *              
 *        Date: Aug 2003
 * 
 * Environment: Xen Minimal OS
 * Description: Random useful library functions, contains some freebsd stuff
 *
 ****************************************************************************
 * $Id: h-insert.h,v 1.4 2002/11/08 16:03:55 rn Exp $
 ****************************************************************************
 *
 *-
 * Copyright (c) 1991, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)stdarg.h    8.1 (Berkeley) 6/10/93
 * $FreeBSD: src/sys/i386/include/stdarg.h,v 1.10 1999/08/28 00:44:26 peter Exp $
 */

#ifndef _STRING_H_
#define _STRING_H_

#include <stddef.h>

/*@ requires \valid_range((char*) cs, 0, count-1)
           ∧ \valid_range((char*) ct, 0, count-1);

    assigns \nothing;

    behavior less:
      assumes ∃ ℤ k; 0 ≤ k < count
                   ∧ ((char*)cs)[k] < ((char*)ct)[k]
                   ∧ ∀ ℤ i; 0 ≤ i < k ⇒ ((char*)cs)[i] ≡ ((char*)ct)[i];

      ensures \result < 0;

    behavior equal:
      assumes ∀ ℤ k; 0 ≤ k < count ⇒ ((char*)cs)[k] ≡ ((char*)ct)[k];
      ensures \result ≡ 0;

    behavior greater:
      assumes ∃ ℤ k; 0 ≤ k < count
                   ∧ ((char*)cs)[k] > ((char*)ct)[k]
                   ∧ ∀ ℤ i; 0 ≤ i < k ⇒ ((char*)cs)[i] ≡ ((char*)ct)[i];

      ensures \result > 0;

    complete behaviors;
    disjoint behaviors;
  @*/
int memcmp(const void *cs, const void *ct, size_t count);


/*@ requires \valid_range((char*)dest, 0, count-1);
    requires \valid_range((char*)src , 0, count-1);

    ensures \result ≡ dest;
    ensures ∀ ℤ i; 0 ≤ i < count ⇒ ((char*)dest)[i] ≡ ((char*)src)[i];
  @*/
void  *memcpy(void *dest, const void *src, size_t count);



int    strncmp(const char *cs, const char *ct, size_t count);



/*@ requires valid_string(cs);
  @ requires valid_string(ct);
  @ behavior less:
  @   assumes ∃ ℤ k; 0 ≤ k < strlen(cs)
  @                ∧ cs[k] < ct[k]
  @                ∧ ∀ ℤ i; 0 ≤ i < k ⇒ cs[i] ≡ ct[i];
  @
  @   ensures \result < 0;
  @
  @ behavior equal:
  @   assumes ∀ ℤ k; 0 ≤ k ≤ strlen(ct) ⇒ cs[k] ≡ ct[k];
  @   assumes ∀ ℤ k; 0 ≤ k ≤ strlen(cs) ⇒ cs[k] ≡ ct[k];
  @   ensures strlen(ct) ≡ strlen(cs);
  @   ensures \result ≡ 0;
  @
  @ behavior greater:
  @   assumes ∃ ℤ k; 0 ≤ k < strlen(ct)
  @                ∧ cs[k] > ct[k]
  @                ∧ ∀ ℤ i; 0 ≤ i < k ⇒ cs[i] ≡ ct[i];
  @
  @   ensures \result > 0;
  @*/
int    strcmp(const char *cs, const char *ct);


/*@ requires valid_string(src);
  @ requires \valid_range(dest,0,strlen(src));
  @
  @ ensures  valid_string(dest);
  @ ensures  \result ≡ dest;
  @*/
char  *strcpy(char *dest, const char *src);


/*@ requires valid_string(src);
  @ requires \valid_range(dest, 0, count-1);
  @ ensures  \result ≡ dest;
 */
char  *strncpy(char *dest, const char *src, size_t count);


/*@ requires \valid_range((char*)s,0,count-1)
           ∧ -128 ≤ c ≤ 127;

    ensures ∀ ℤ i; 0 ≤ i < count ⇒ ((char*)s)[i] ≡ c
          ∧ \result ≡ s;
 */
void  *memset(void *s,int c, size_t count);


/*@ requires valid_string(s);
    ensures \result ≡ minimum (strlen(s), count);
  @*/
size_t strnlen(const char *s, size_t count);


/*@ requires valid_string(s);
    ensures \result ≡ strlen(s);
  */
size_t strlen(const char *s);


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
char  *strchr(const char *s, int c);



/*@ requires valid_string(s);
  @ requires -128 ≤ c < 0 ∨ 0 < c ≤ 127;
  @
  @ behavior success:
  @  assumes ∃ ℤ k; 0 ≤ k < strlen(s) ∧ s[k] ≡ c;
  @  ensures *\result ≡ c
  @        ∧ valid_string(\result);
  @
  @ behavior failure:
  @  assumes ∀ ℤ i; 0 ≤ i < strlen(s) ⇒ s[i] ≢ c;
  @  ensures \result ≡ \null;
  @
  @*/
char  *strrchr(const char *s, int c);



char  *strstr(const char *s1, const char *s2);

/*@ requires valid_string(dest);
  @ requires valid_string(src);
  @ requires strlen(dest) + strlen(src) < 0xffffffffffffffff;
  @ requires \valid_range(dest,0, strlen(dest) + strlen(src) + 1);
  @
  @ ensures valid_string(dest);
  @ ensures \result ≡ dest;
  @*/
char  *strcat(char * dest, const char * src);

void *memchr(const void *s, int c, size_t n);

#endif
