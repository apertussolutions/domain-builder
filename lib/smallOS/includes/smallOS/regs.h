//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_REGS_H__
#define __SMALLOS_REGS_H__

typedef struct {
// bellow: preserved across calls, but not in context.
  unsigned long r15;
  unsigned long r14;
  unsigned long r13;
  unsigned long r12;
  unsigned long rbp;
  unsigned long rbx;
// bellow: not preserved across calls
  unsigned long r10;
  unsigned long r9;
  unsigned long r8;
  unsigned long rax;
  unsigned long rdx;
  unsigned long rsi;
  unsigned long rdi;
  unsigned long rcx;
  unsigned long r11;
// below: cpu exception frame
  unsigned long rip;
  unsigned long cs;
  unsigned long eflags;
  unsigned long rsp;
  unsigned long ss;
// top of stack page
} pt_regs;

#endif

