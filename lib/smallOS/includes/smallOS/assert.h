//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#ifndef __SMALLOS_ASSERT_H__
#define __SMALLOS_ASSERT_H__

void assert_full(int condition, const char * file
		 , const char * func, int line);

#define assert(cond) assert_full(cond, __FILE__, __FUNCTION__, __LINE__)

#endif


