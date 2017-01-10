// BANNERSTART
// Copyright: 2006-2008, Galois, Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki
// BANNEREND
#ifndef __STDDEF_H__
#define __STDDEF_H__

#if defined(__cplusplus)
#define NULL 0
#else
#define NULL ((void *)0)
#endif

typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned short wchar_t;
typedef unsigned long long uintptr_t;

#define offsetof(st, m) ((size_t) ( (char *)&((st *)(0))->m - (char *)0 ))

#endif
